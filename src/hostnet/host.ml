open Lwt.Infix

let src =
  let src = Logs.Src.create "Luv" ~doc:"Host interface based on Luv" in
  Logs.Src.set_level src (Some Logs.Info);
  src

module Log = (val Logs.src_log src : Logs.LOG)

module Result = struct
  include Result
  let get_ok = function
    | Error _ -> invalid_arg "result is Error _"
    | Ok x -> x
end

let log_exception_continue description f =
  let to_string = function
    | Failure x -> x
    | e -> Printexc.to_string e in
  Lwt.catch
    (fun () -> f ())
    (fun e ->
       Log.warn (fun f -> f "%s: %s" description (to_string e));
       Lwt.return ()
    )

let make_sockaddr (ip, port) = match ip with
  | Ipaddr.V4 _ -> Luv.Sockaddr.ipv4 (Ipaddr.to_string ip) port
  | Ipaddr.V6 _ -> Luv.Sockaddr.ipv6 (Ipaddr.to_string ip) port

let parse_sockaddr sockaddr =
  match Luv.Sockaddr.to_string sockaddr, Luv.Sockaddr.port sockaddr with
  | Some ip, Some port -> Ok (Ipaddr.of_string_exn ip, port)
  | None, _ -> Error `UNKNOWN
  | _, None -> Error `UNKNOWN

let string_of_address (dst, dst_port) =
  Ipaddr.to_string dst ^ ":" ^ (string_of_int dst_port)

module Common = struct
  (** FLOW boilerplate *)

  type 'a io = 'a Lwt.t
  type buffer = Cstruct.t
  type error = [`Msg of string]
  type write_error = [Mirage_flow.write_error | error]
  let pp_error ppf (`Msg x) = Fmt.string ppf x

  let pp_write_error ppf = function
  | #Mirage_flow.write_error as e -> Mirage_flow.pp_write_error ppf e
  | #error as e                   -> pp_error ppf e

  let errorf fmt = Fmt.kstrf (fun s -> Lwt_result.fail (`Msg s)) fmt

end

module Sockets = struct

  let max_connections = ref None

  let set_max_connections x =
    begin match x with
      | None -> Log.info (fun f -> f "Removed connection limit")
      | Some limit -> Log.info (fun f -> f "Updated connection limit to %d" limit)
    end;
    max_connections := x

  let next_connection_idx =
    let idx = ref 0 in
    fun () ->
      let next = !idx in
      incr idx;
      next

  exception Too_many_connections

  let connection_table = Hashtbl.create 511
  let get_num_connections () = Hashtbl.length connection_table

  let connections () =
    let xs = Hashtbl.fold (fun _ c acc -> c :: acc) connection_table [] in
    Vfs.File.ro_of_string (String.concat "\n" xs)

  let register_connection_no_limit description =
    let idx = next_connection_idx () in
    Hashtbl.replace connection_table idx description;
    idx

  let register_connection =
    let last_error_log = ref 0. in
    fun description -> match !max_connections with
    | Some m when Hashtbl.length connection_table >= m ->
      let now = Unix.gettimeofday () in
      if (now -. !last_error_log) > 30. then begin
        (* Avoid hammering the logging system *)
        Log.warn (fun f ->
            f "Exceeded maximum number of forwarded connections (%d)" m);
        last_error_log := now;
      end;
      Lwt.fail Too_many_connections
    | _ ->
      let idx = register_connection_no_limit description in
      Lwt.return idx

  let register_connection_noexn description =
    Lwt.catch (fun () -> register_connection description >>= fun idx -> Lwt.return (Some idx)) (fun _ -> Lwt.return None)

  let deregister_connection idx =
    if not(Hashtbl.mem connection_table idx) then begin
      Log.warn (fun f -> f "Deregistered connection %d more than once" idx)
    end;
    Hashtbl.remove connection_table idx

  let file_of_file_descr fd =
    (* FIXME: this is Unix-only. It would be better to pass an fd over a pipe instead. *)
    let os_fd : Luv.Os_fd.Fd.t = Obj.magic fd in
    Luv.File.open_osfhandle os_fd

  module Datagram = struct
    type address = Ipaddr.t * int

    module Udp = struct
      include Common

      type flow = {
        idx: int option;
        label: string;
        description: string;
        mutable fd: Luv.UDP.t option;
        mutable already_read: Cstruct.t option;
        sockaddr: Luv.Sockaddr.t;
        address: address;
      }

      type address = Ipaddr.t * int

      let string_of_flow t = Fmt.strf "udp -> %s" (string_of_address t.address)

      let of_fd
          ?idx ?read_buffer_size:_
          ?(already_read = None) ~description sockaddr address fd
        =
        let label = match fst address with
        | Ipaddr.V4 _ -> "UDPv4"
        | Ipaddr.V6 _ -> "UDPv6" in
        { idx; label; description; fd = Some fd; already_read;
          sockaddr; address }

      let connect ?read_buffer_size address =
        let description = "udp:" ^ (string_of_address address) in
        let label = match address with
        | Ipaddr.V4 _, _ -> "UDPv4"
        | Ipaddr.V6 _, _ -> "UDPv6" in
        register_connection description
        >>= fun idx ->
        Luv_lwt.in_luv (fun return ->
          begin match Luv.UDP.init () with
          | Error err ->
            return (Error (`Msg (Luv.Error.strerror err)))
          | Ok fd ->
            let any_result = match address with
              | Ipaddr.V4 _, _ -> Luv.Sockaddr.ipv4 "0" 0
              | Ipaddr.V6 _, _ -> Luv.Sockaddr.ipv6 "0" 0 in
            begin match any_result with
            | Error err ->
              return (Error (`Msg (Luv.Error.strerror err)))
            | Ok any ->
              begin match Luv.UDP.bind fd any with
              | Error err ->
                Luv.Handle.close fd (fun () -> return (Error (`Msg (Luv.Error.strerror err))))
              | Ok () ->
                begin match make_sockaddr address with
                | Error err ->
                  Luv.Handle.close fd (fun () -> return (Error (`Msg (Luv.Error.strerror err))))
                | Ok sockaddr ->
                  return (Ok (fd, sockaddr))
                end
              end
            end
          end
      ) >>= function
      | Error (`Msg m) ->
        deregister_connection idx;
        let msg = Fmt.strf "Socket.%s.connect %s: %s" label (string_of_address address) m in
        Log.err (fun f -> f "%s" msg);
        Lwt.return (Error (`Msg msg))
      | Ok (fd, sockaddr) ->
        Lwt.return (Ok (of_fd ~idx ?read_buffer_size ~description sockaddr address fd))

      let read t = match t.fd, t.already_read with
      | None, _ -> Lwt.return (Ok `Eof)
      | Some _, Some data when Cstruct.len data > 0 ->
        t.already_read <- Some (Cstruct.sub data 0 0); (* next read is `Eof *)
        Lwt.return (Ok (`Data data))
      | Some _, Some _ ->
        Lwt.return (Ok `Eof)
      | Some fd, None ->
        Luv_lwt.in_luv (fun return ->
          Luv.UDP.recv_start fd begin function
          | Error err ->
            return (Error (`Msg (Luv.Error.strerror err)))
          | Ok (_, None, _) -> () (* EAGAIN, to be ignored *)
          | Ok (buf, Some peer, flags) ->
            if List.mem `PARTIAL flags then begin
              Log.err (fun f ->
                f "Socket.%s.read: dropping partial response (buffer \
                  was %d bytes)" t.label (Luv.Buffer.size buf));
            end else begin
              begin match parse_sockaddr peer with
                | Error _ ->
                  Log.warn (fun f ->
                    f "Socket.%s.read: dropping response from unknown peer" t.label
                  )
                | Ok address when address <> t.address ->
                  Log.warn (fun f ->
                    f "Socket.%s.read: dropping response from %s since \
                      we're connected to %s" t.label
                      (string_of_address address)
                      (string_of_address t.address)
                  )
                | Ok _ ->
                  (* We got one! *)
                  begin match Luv.UDP.recv_stop fd with
                  | Error err ->
                    return (Error (`Msg (Luv.Error.strerror err)))
                  | Ok () -> return (Ok (`Data (Cstruct.of_bigarray buf)))
                  end
              end
            end
          end
        )

      let writev t bufs = match t.fd with
      | None -> Lwt.return (Error `Closed)
      | Some fd ->
        let buffers = List.map (fun buf -> Luv.Buffer.sub buf.Cstruct.buffer ~offset:buf.Cstruct.off ~length:buf.Cstruct.len) bufs in
        Luv_lwt.in_luv (fun return ->
          Luv.UDP.send fd buffers t.sockaddr begin function
            | Error err ->
              return (Error (`Msg (Luv.Error.strerror err)))
            | Ok () ->
              return (Ok ())
          end
        )

      let write t buf = writev t [ buf ]

      let close t = match t.fd with
      | None -> Lwt.return_unit
      | Some fd ->
        t.fd <- None;
        Log.debug (fun f -> f "Socket.%s.close: %s" t.label (string_of_flow t));
        Luv_lwt.in_luv (Luv.Handle.close fd)
        >>= fun () ->
        begin match t.idx with
        | Some idx -> deregister_connection idx
        | None -> ()
        end;
        Lwt.return_unit

      let shutdown_read _t = Lwt.return_unit
      let shutdown_write _t = Lwt.return_unit

      type server = {
        idx: int;
        label: string;
        fd: Luv.UDP.t;
        fd_mutex: Lwt_mutex.t;
        mutable closed: bool;
        mutable disable_connection_tracking: bool;
      }

      let make ~idx ~label fd =
        let fd_mutex = Lwt_mutex.create () in
        { idx; label; fd; fd_mutex; closed = false; disable_connection_tracking = false }

      let disable_connection_tracking server =
        server.disable_connection_tracking <- true

      let bind ?(description="") (ip, port) =
        let label = match ip with
        | Ipaddr.V4 _ -> "UDPv4"
        | Ipaddr.V6 _ -> "UDPv6" in
        let description =
          Fmt.strf "udp:%a:%d %s" Ipaddr.pp_hum ip port description
        in
        register_connection description >>= fun idx ->
        Luv_lwt.in_luv (fun return ->
          match Luv.UDP.init () with
          | Error err -> return (Error (`Msg (Luv.Error.strerror err)))
          | Ok fd ->
            begin match make_sockaddr(ip, port) with
            | Error err ->
              Luv.Handle.close fd (fun () -> return (Error (`Msg (Luv.Error.strerror err))))
            | Ok sockaddr ->
              begin match Luv.UDP.bind ~reuseaddr:true fd sockaddr with
              | Error err ->
                Luv.Handle.close fd (fun () -> return (Error (`Msg (Luv.Error.strerror err))))
              | Ok () ->
                return (Ok fd)
              end
            end
        ) >>= function
        | Error (`Msg m) ->
          deregister_connection idx;
          let msg = Fmt.strf "Socket.%s.bind %s:%d: %s" label (Ipaddr.to_string ip) port m in
          Log.err (fun f -> f "%s" msg);
          Lwt.fail_with msg
        | Ok fd ->
          Lwt.return (make ~idx ~label fd)

      let of_bound_fd ?read_buffer_size:_ _fd =
        failwith "UDP.of_bound_fd not implemented"

      let getsockname { fd; _ } =
        Luv_lwt.in_luv (fun return ->
          match Luv.UDP.getsockname fd with
          | Error err -> return (Error (`Msg (Luv.Error.strerror err)))
          | Ok sockaddr ->
            begin match parse_sockaddr sockaddr with
            | Error err -> return (Error (`Msg (Luv.Error.strerror err)))
            | Ok (ip, port) -> return (Ok (ip, port))
            end
        ) >>= function
        | Error (`Msg m) -> Lwt.fail_with m
        | Ok x -> Lwt.return x

      let shutdown server =
        if not server.closed then begin
          server.closed <- true;
          Luv_lwt.in_luv (Luv.Handle.close server.fd)
          >>= fun () ->
          deregister_connection server.idx;
          Lwt.return_unit
        end else
          Lwt.return_unit

      let recvfrom server buf =
        let buf = Luv.Buffer.sub buf.Cstruct.buffer ~offset:buf.Cstruct.off ~length:buf.Cstruct.len in
        Luv_lwt.in_luv (fun return ->
          Luv.UDP.recv_start ~allocate:(fun _ -> buf) server.fd begin function
          | Error err -> return (Error (`Msg (Luv.Error.strerror err)))
          | Ok (_, None, _) -> () (* EAGAIN, to be ignored *)
          | Ok (buf, Some peer, flags) ->
            if List.mem `PARTIAL flags then begin
              Log.err (fun f ->
                f "Socket.%s.read: dropping partial response (buffer \
                  was %d bytes)" server.label (Luv.Buffer.size buf));
            end else begin
              begin match parse_sockaddr peer with
                | Error _ ->
                  Log.warn (fun f ->
                    f "Socket.%s.read: dropping response from unknown peer" server.label
                  )
                | Ok address ->
                  (* We got one! *)
                  begin match Luv.UDP.recv_stop server.fd with
                  | Error err -> return (Error (`Msg (Luv.Error.strerror err)))
                  | Ok () -> return (Ok (Luv.Buffer.size buf, address))
                  end
              end
            end
          end
        ) >>= function
        | Error (`Msg m) -> Lwt.fail_with m
        | Ok (size, address) -> Lwt.return (size, address)

      let listen t flow_cb =
        let rec loop () =
          Lwt.catch (fun () ->
              (* Allocate a fresh buffer because the packet will be
                 processed in a background thread *)
              let buffer = Cstruct.create Constants.max_udp_length in
              recvfrom t buffer
              >>= fun (n, address) ->
              let data = Cstruct.sub buffer 0 n in
              (* construct a flow with this buffer available for reading *)
              (* No new fd so no new idx *)
              let description = Fmt.strf "udp:%s" (string_of_address address) in
              match make_sockaddr address with
              | Error _ ->
                Log.warn (fun f ->
                  f "Socket.%s.listen: dropping response from unknown peer" t.label
                );
                Lwt.return true
              | Ok sockaddr ->
                let flow =
                  of_fd ~description ~read_buffer_size:0 ~already_read:(Some data)
                    sockaddr address t.fd
                in
                Lwt.async (fun () ->
                    Lwt.catch
                      (fun () -> flow_cb flow)
                      (fun e ->
                        Log.info (fun f -> f "Socket.%s.listen callback caught: %s"
                                      t.label (Printexc.to_string e)
                                  );
                        Lwt.return_unit
                      )
                  );
                Lwt.return true
            ) (fun e ->
              Log.err (fun f -> f "Socket.%s.listen caught %s shutting down server"
                          t.label(Printexc.to_string e)
                      );
              Lwt.return false
            )
          >>= function
          | false -> Lwt.return_unit
          | true -> loop ()
        in
        Lwt.async loop

      let sendto server (ip, port) ?(ttl=64) buf =
        (* Avoid a race between the setSocketTTL and the send_ba *)
        Lwt_mutex.with_lock server.fd_mutex
          (fun () ->
            let buf = Luv.Buffer.sub buf.Cstruct.buffer ~offset:buf.Cstruct.off ~length:buf.Cstruct.len in
            Luv_lwt.in_luv (fun return ->
              match make_sockaddr (ip, port) with
              | Error err ->
                return (Error (`Msg (Luv.Error.strerror err)))
              | Ok sockaddr ->
                begin match Luv.UDP.set_ttl server.fd ttl with
                | Error err ->
                  return (Error (`Msg (Luv.Error.strerror err)))
                | Ok () ->
                  Luv.UDP.send server.fd [ buf ] sockaddr begin function
                    | Error err ->
                      return (Error (`Msg (Luv.Error.strerror err)))
                    | Ok () ->
                      return (Ok ())
                  end
                end
            )
          )
        >>= function
        | Error (`Msg m) ->
          let msg = Fmt.strf "%s.sendto %s: %s" server.label (string_of_address (ip, port)) m in
          Log.err (fun f -> f "%s" msg);
          Lwt.fail_with m
        | Ok () -> Lwt.return_unit
    end

  end

  module Stream = struct

    (* Common across TCP and Pipes *)

    let read_into fd buf =
      Luv_lwt.in_luv (fun return ->
        let buffer = Luv.Buffer.sub buf.Cstruct.buffer ~offset:buf.Cstruct.off ~length:buf.Cstruct.len in
        Luv.Stream.read_start ~allocate:(fun _suggested -> buffer) fd begin function
          | Ok b ->
            let n = Luv.Buffer.size buffer - (Luv.Buffer.size b) in
            if n == 0 then begin
              match Luv.Stream.read_stop fd with
              | Ok () -> return (Ok (`Data ()))
              | Error err ->return (Error (`Msg (Luv.Error.strerror err)))
            end
          | Error err ->
            return (Error (`Msg (Luv.Error.strerror err)))
        end
      )

    let read fd =
      Luv_lwt.in_luv (fun return ->
        Luv.Stream.read_start fd begin function
        | Ok buf ->
          begin match Luv.Stream.read_stop fd with
          | Ok () -> return (Ok (`Data (Cstruct.of_bigarray buf)))
          | Error err -> return (Error (`Msg (Luv.Error.strerror err)))
          end
        | Error err ->
          return (Error (`Msg (Luv.Error.strerror err)))
        end
      )

    let writev fd bufs =
      let buffers = List.map (fun buf -> Luv.Buffer.sub buf.Cstruct.buffer ~offset:buf.Cstruct.off ~length:buf.Cstruct.len) bufs in
      Luv_lwt.in_luv (fun return ->
        let rec loop buffers =
          if Luv.Buffer.total_size buffers == 0
          then return (Ok ())
          else Luv.Stream.write fd buffers begin fun r n -> match r with
          | Error err ->
            return (Error (`Msg (Luv.Error.strerror err)))
          | Ok () ->
            loop (Luv.Buffer.drop buffers n)
          end in
        loop buffers
      )

    module Tcp = struct
      include Common

      type address = Ipaddr.t * int

      type flow = {
        idx: int;
        label: string;
        description: string;
        fd: Luv.TCP.t;
        mutable closed: bool;
      }

      let of_fd ~label
          ~idx ?read_buffer_size:_ ~description fd
        =
        let closed = false in
        { idx; label; description; fd; closed }

      let connect ?read_buffer_size:_ (ip, port) =
        let description = Fmt.strf "tcp:%a:%d" Ipaddr.pp_hum ip port in
        let label = match ip with
        | Ipaddr.V4 _ -> "TCPv4"
        | Ipaddr.V6 _ -> "TCPv6" in

        register_connection_noexn description
        >>= function
        | None ->
          errorf "Socket.%s.connect %s: hit connection limit" label description
        | Some idx ->
          begin Luv_lwt.in_luv (fun return ->
            match Luv.TCP.init () with
            | Error err -> return (Error (`Msg (Luv.Error.strerror err)))
            | Ok fd ->
              begin match make_sockaddr (ip, port) with
              | Error err ->
                Luv.Handle.close fd ignore;
                return (Error (`Msg (Luv.Error.strerror err)))
              | Ok sockaddr ->
                Luv.TCP.connect fd sockaddr begin function
                | Error err ->
                  Luv.Handle.close fd ignore;
                  return (Error (`Msg (Luv.Error.strerror err)))
                | Ok () ->
                  return (Ok fd)
                end
              end
          ) >>= function
          | Error (`Msg m) ->
            deregister_connection idx;
            let msg = Fmt.strf "Socket.%s.connect %s:%d: %s" label (Ipaddr.to_string ip) port m in
            Log.err (fun f -> f "%s" msg);
            Lwt.return (Error (`Msg msg))
          | Ok fd ->
            Lwt.return (Ok (of_fd ~description ~idx ~label fd))
          end

      let shutdown_read _ =
        Lwt.return ()

      let shutdown_write { label; fd; closed; _ } =
        if not closed
          then
            Luv_lwt.in_luv (fun return ->
              Luv.Stream.shutdown fd begin function
                | Error err ->
                  Log.err (fun f -> f "Socket.%s.shutdown_write: %s" label (Luv.Error.strerror err));
                  return ()
                | Ok () ->
                  return ()
              end
            )
          else Lwt.return_unit

      let read_into t buf = read_into t.fd buf
      let read t = read t.fd
      let writev t bufs = writev t.fd bufs
      let write t buf = writev t [ buf ]

      let close t =
        if not t.closed then begin
          t.closed <- true;
          Luv_lwt.in_luv (Luv.Handle.close t.fd)
          >>= fun () ->
          deregister_connection t.idx;
          Lwt.return_unit
        end else Lwt.return_unit

      type server = {
        label: string;
        mutable listening_fds: (int * Luv.TCP.t) list;
        mutable disable_connection_tracking: bool;
      }

      let getsockname_in_luv fd = match Luv.TCP.getsockname fd with
        | Error _ -> invalid_arg "Tcp.getsockname passed a non-TCP socket"
        | Ok sockaddr ->
          begin match parse_sockaddr sockaddr with
          | Error _ -> invalid_arg "Tcp.getsockname unable to parse Sockaddr.t"
          | Ok (ip, port) -> ip, port
          end

      let label_of ip = match ip with
        | Ipaddr.V4 _ -> "TCPv4"
        | Ipaddr.V6 _ -> "TCPv6"

      let make ?read_buffer_size:_ listening_fds =
        ( match listening_fds with
          | [] -> Lwt.fail_with "socket is closed"
          | (_, fd) :: _ -> Lwt.return fd )
        >>= fun fd ->
        Luv_lwt.in_luv (fun return -> return @@ getsockname_in_luv fd)
        >>= fun sockname ->
        let label = label_of @@ fst sockname in
        Lwt.return { label; listening_fds;
          disable_connection_tracking = false }

      let disable_connection_tracking server =
        server.disable_connection_tracking <- true

      let getsockname server =
        ( match server.listening_fds with
          | [] -> Lwt.fail_with "socket is closed"
          | (_, fd) :: _ -> Lwt.return fd )
        >>= fun fd ->
        Luv_lwt.in_luv (fun return -> return @@ getsockname_in_luv fd)

      let bind_one ?(description="") (ip, port) =
        let label = match ip with
        | Ipaddr.V4 _ -> "TCPv4"
        | Ipaddr.V6 _ -> "TCPv6" in
        let description =
          Fmt.strf "tcp:%a:%d %s" Ipaddr.pp_hum ip port description
        in
        register_connection description >>= fun idx ->
        Luv_lwt.in_luv (fun return ->
          match Luv.TCP.init() with
          | Error err -> return (Error (`Msg (Luv.Error.strerror err)))
          | Ok fd ->
            begin match make_sockaddr (ip, port) with
            | Error err ->
              Luv.Handle.close fd (fun () -> return (Error (`Msg (Luv.Error.strerror err))))
            | Ok sockaddr ->
              begin match Luv.TCP.bind fd sockaddr with
              | Error err ->
                Luv.Handle.close fd (fun () -> return (Error (`Msg (Luv.Error.strerror err))))
              | Ok () ->
                (* Determine the local port number if the user requested INADDR_ANY *)
                begin match Luv.TCP.getsockname fd with
                | Error err ->
                  Luv.Handle.close fd (fun () -> return (Error (`Msg (Luv.Error.strerror err))))
                | Ok sockaddr ->
                  begin match parse_sockaddr sockaddr with
                  | Error err ->
                    Luv.Handle.close fd (fun () -> return (Error (`Msg (Luv.Error.strerror err))))
                  | Ok (_, port) -> return (Ok (idx, label, fd, port))
                  end
                end
              end
            end
        ) >>= function
        | Error (`Msg m) ->
          deregister_connection idx;
          let msg = Fmt.strf "Socket.%s.bind_one %s:%d: %s" label (Ipaddr.to_string ip) port m in
          Log.err (fun f -> f "%s" msg);
          Lwt.return (Error (`Msg m))
        | Ok x ->
          Lwt.return (Ok x)

      let bind ?description (ip, port) =
        bind_one ?description (ip, port)
        >>= function
        | Error (`Msg m) -> Lwt.fail_with m
        | Ok (idx, _label, fd, local_port) ->
        (* On some systems localhost will resolve to ::1 first and this can
           cause performance problems (particularly on Windows). Perform a
           best-effort bind to the ::1 address. *)
        Lwt.catch (fun () ->
            if Ipaddr.compare ip (Ipaddr.V4 Ipaddr.V4.localhost) = 0
            || Ipaddr.compare ip (Ipaddr.V4 Ipaddr.V4.any) = 0
            then begin
              Log.debug (fun f ->
                  f "Attempting a best-effort bind of ::1:%d" local_port);
              bind_one (Ipaddr.(V6 V6.localhost), local_port)
              >>= function
              | Error (`Msg m) -> Lwt.fail_with m
              | Ok (idx, _, fd, _) ->
                Lwt.return [ idx, fd ]
            end else
              Lwt.return []
          ) (fun e ->
            Log.debug (fun f ->
                f "Ignoring failed bind to ::1:%d (%a)" local_port Fmt.exn e);
            Lwt.return []
          )
        >>= fun extra ->
        make ((idx, fd) :: extra)

      let shutdown server =
        let fds = server.listening_fds in
        server.listening_fds <- [];
        Lwt_list.iter_s (fun (idx, fd) ->
          Luv_lwt.in_luv (Luv.Handle.close fd)
          >>= fun () ->
          deregister_connection idx;
          Lwt.return_unit
        ) fds

      let of_bound_fd ?read_buffer_size:_ _fd =
        failwith "TCP.of_bound_fd not implemented"

      let listen server' cb =
        let handle_connection client label description =
          (if server'.disable_connection_tracking
           then Lwt.return @@
             register_connection_no_limit description
           else register_connection description )
          >>= fun idx ->
          let flow = of_fd ~label ~idx ~description client in
          Lwt.finalize (fun () ->
            log_exception_continue "TCP.listen"
              (fun () -> cb flow)
          ) (fun () -> close flow ) in

        List.iter (fun (_, fd) ->
          Luv_lwt.in_luv_async (fun () ->
            let ip, port = getsockname_in_luv fd in
            let label = label_of ip in
            let description = Fmt.strf "%s:%s:%d" label (Ipaddr.to_string ip) port in
            Luv.Stream.listen fd begin function
            | Error err -> Log.err (fun f -> f "TCP.listen: %s"  (Luv.Error.strerror err))
            | Ok () ->
              let rec accept_forever () =
                match Luv.TCP.init () with
                | Error err -> Log.err (fun f -> f "TCP.init: %s"  (Luv.Error.strerror err))
                | Ok client ->
                  let error msg err =
                    Log.err (fun f -> f "Socket.%s.listen %s: %s" label msg (Luv.Error.strerror err));
                    Luv.Handle.close client ignore in
                  begin match Luv.Stream.accept ~server:fd ~client with
                  | Error err -> error "accept" err
                  | Ok () ->
                    begin match Luv.TCP.nodelay client true with
                    | Error err -> error "nodelay" err
                    | Ok () ->
                      begin match Luv.TCP.keepalive client (Some 1) with
                      | Error err -> error "keepalive" err
                      | Ok () ->
                        Luv_lwt.in_lwt_async (fun () -> Lwt.async (fun () -> handle_connection client label description));
                        accept_forever ()
                      end
                    end
                  end in
              accept_forever ()
            end
          )
        ) server'.listening_fds
    end

    module Unix = struct
      include Common

      type address = string

      let get_test_address () =
        let i = Random.int 1_000_000 in
        if Sys.os_type == "Windows"
        then Printf.sprintf "\\\\.\\pipe\\vpnkittest%d" i
        else Printf.sprintf "/tmp/vpnkittest.%d" i

      type flow = {
        idx: int;
        description: string;
        fd: Luv.Pipe.t;
        mutable closed: bool;
      }

      let of_fd
          ~idx ?read_buffer_size:_ ~description fd
        =
        let closed = false in
        { idx; description; fd; closed }

      let unsafe_get_raw_fd _t =
        failwith "unsafe_get_raw_fd unimplemented"

      let connect ?read_buffer_size:_ path =
        let description = "unix:" ^ path in
        register_connection description
        >>= fun idx ->
        Luv_lwt.in_luv (fun return ->
          match Luv.Pipe.init () with
          | Error err ->
            let msg = Fmt.strf "Pipe.connect %s: %s" path (Luv.Error.strerror err) in
            Log.err (fun f -> f "%s" msg);
            return (Error (`Msg msg))
          | Ok fd ->
            Luv.Pipe.connect fd path begin function
            | Error err -> return (Error (`Msg (Luv.Error.strerror err)))
            | Ok () -> return (Ok (of_fd ~description ~idx fd))
            end
        ) >>= function
        | Error e ->
          deregister_connection idx;
          Lwt.return (Error e)
        | Ok x ->
          Lwt.return (Ok x)

      let shutdown_read _ =
        Lwt.return ()

      let shutdown_write { fd; closed; _ } =
        if not closed
        then
          Luv_lwt.in_luv (fun return ->
            Luv.Stream.shutdown fd begin function
              | Error err ->
                Log.err (fun f -> f "Pipe.shutdown_write: %s" (Luv.Error.strerror err));
                return ()
              | Ok () ->
                return ()
            end
          )
        else Lwt.return_unit

      let read_into t buf = read_into t.fd buf
      let read t = read t.fd
      let writev t bufs = writev t.fd bufs
      let write t buf = writev t [ buf ]

      let close t =
        if not t.closed then begin
          t.closed <- true;
          Luv_lwt.in_luv (Luv.Handle.close t.fd)
          >>= fun () ->
          deregister_connection t.idx;
          Lwt.return_unit
        end else Lwt.return_unit

      type server = {
        idx: int;
        fd: Luv.Pipe.t;
        mutable closed: bool;
        mutable disable_connection_tracking: bool;
      }

      let bind ?(description="") path =
        Luv_lwt.in_luv (fun return ->
            Luv.File.unlink path return
        ) >>= fun _ ->
        let description = Fmt.strf "unix:%s %s" path description in
        register_connection description >>= fun idx ->
        Luv_lwt.in_luv (fun return ->
          match Luv.Pipe.init () with
          | Error err -> return (Error (`Msg (Luv.Error.strerror err)))
          | Ok fd ->
            begin match Luv.Pipe.bind fd path with
            | Ok () ->
              return (Ok { idx; fd; closed = false; disable_connection_tracking = false })
            | Error err ->
              return (Error (`Msg (Luv.Error.strerror err)))
            end
        ) >>= function
        | Error (`Msg m) ->
          deregister_connection idx;
          Lwt.fail_with m
        | Ok x -> Lwt.return x

      let getsockname server =
        Luv_lwt.in_luv (fun return -> return (Luv.Pipe.getsockname server.fd))
        >>= function
        | Ok path -> Lwt.return path
        | _ -> Lwt.fail (Invalid_argument "Unix.sockname passed a non-Unix socket")

      let disable_connection_tracking server =
        server.disable_connection_tracking <- true

      let listen ({ fd; _ } as server') cb =
        let handle_connection client description =
          (if server'.disable_connection_tracking
           then Lwt.return @@
             register_connection_no_limit description
           else register_connection description )
          >>= fun idx ->
          let flow = of_fd ~idx ~description client in
          Lwt.finalize (fun () ->
            log_exception_continue "Pipe.listen"
              (fun () -> cb flow)
          ) (fun () -> close flow ) in

        Luv_lwt.in_luv_async (fun () ->
          let description = "unix:" ^ (match Luv.Pipe.getsockname fd with
            | Ok path -> path
            | Error err -> "(error " ^ (Luv.Error.strerror err) ^ ")") in
          Luv.Stream.listen fd begin function
          | Error err -> Log.err (fun f -> f "Pipe.listen: %s"  (Luv.Error.strerror err))
          | Ok () ->
            let rec accept_forever () =
              match Luv.Pipe.init () with
              | Error err -> Log.err (fun f -> f "Pipe.init: %s"  (Luv.Error.strerror err))
              | Ok client ->
                begin match Luv.Stream.accept ~server:fd ~client with
                | Error err -> Log.err (fun f -> f "Pipe.accept: %s" (Luv.Error.strerror err))
                | Ok () ->
                  Luv_lwt.in_lwt_async (fun () -> Lwt.async (fun () -> handle_connection client description));
                  accept_forever ()
                end in
            accept_forever ()
          end
        )

      let of_bound_fd ?read_buffer_size:_ fd =
        Luv_lwt.in_luv (fun return ->
          match file_of_file_descr fd with
          | Error err -> return (Error (`Msg (Luv.Error.strerror err)))
          | Ok file ->
            let fd = Luv.Pipe.init () |> Result.get_ok in
            begin match Luv.Pipe.open_ fd file with
            | Error err ->
              Luv.Handle.close fd ignore;
              return (Error (`Msg (Luv.Error.strerror err)))
            | Ok () ->
              let description = match Luv.Pipe.getsockname fd with
                | Ok path -> "unix:" ^ path
                | Error err -> "getsockname failed: " ^ (Luv.Error.strerror err) in
              return (Ok (description, fd))
            end
        ) >>= function
        | Error (`Msg m) ->
          Log.err (fun f -> f "%s" m);
          failwith m
        | Ok (description, fd) ->
          let idx = register_connection_no_limit description in
          Lwt.return { idx; fd; closed = false; disable_connection_tracking = false }

      let shutdown server =
        if not server.closed then begin
          server.closed <- true;
          Luv_lwt.in_luv (Luv.Handle.close server.fd)
          >>= fun () ->
          deregister_connection server.idx;
          Lwt.return_unit
        end else
          Lwt.return_unit
    end
  end
end

module type ClientServer = sig
  include Sig.FLOW_CLIENT
  include Sig.FLOW_SERVER
    with type address := address
    and type flow := flow
  val get_test_address: unit -> address
end

module TestServer(F: ClientServer) = struct
  let one_connection () =
    Luv_lwt.run begin
      let address = F.get_test_address () in
      F.bind address
      >>= fun server ->
      let connected = Lwt_mvar.create () in
      F.listen server (fun flow ->
        Lwt_mvar.put connected ()
        >>= fun () ->
        F.close flow
      );
      Lwt_mvar.take connected
      >>= fun () ->
      F.shutdown server
    end
end

let%test_module "Sockets.Stream.Unix" = (module struct
  module Tests = TestServer(Sockets.Stream.Unix)
  let%test_unit "one Unix/Pipe connection" = Tests.one_connection ()
end)


module Files = struct
  let read_file path =
    (* Caller wants a string *)
    let buf = Buffer.create 4096 in
    let frag = Luv.Buffer.create 4096 in
    Luv_lwt.in_luv (fun return ->
      let rec loop h =
        Luv.File.read h [ frag ]
          (function
            | Ok n ->
              if n = Unsigned.Size_t.zero then begin
                Luv.File.close h begin function
                | Error err -> return (Error (`Msg (Luv.Error.strerror err)))
                | Ok () -> return (Ok (Buffer.contents buf))
                end
              end else begin
                Luv.Buffer.(sub frag ~offset:0 ~length:(Unsigned.Size_t.to_int n) |> to_bytes) |> Buffer.add_bytes buf;
                loop h
              end
            | Error err ->
              Luv.File.close h begin function
              | Error err -> return (Error (`Msg (Luv.Error.strerror err)))
              | Ok () -> return (Error (`Msg (Luv.Error.strerror err)))
              end
          ) in
      Luv.File.open_ path [ `RDONLY ]
        (function
        | Error err ->
          return (Error (`Msg (Luv.Error.strerror err)))
        | Ok h ->
          loop h
        )
    )

  let%test "read a file" =
    let expected = Buffer.create 8192 in
    for i = 0 to 1024 do
      Buffer.add_int64_be expected (Int64.of_int i)
    done;
    let filename = Filename.temp_file "vpnkit" "file" in
    let oc = open_out_bin filename in
    output_string oc (Buffer.contents expected);
    close_out oc;
    Luv_lwt.run begin
      read_file filename
      >>= fun result ->
      Sys.remove filename;
      match result with
      | Error (`Msg m) -> failwith m
      | Ok actual -> Lwt.return (Buffer.contents expected = actual)
    end

  type watch = {
    h: [ `FS_event ] Luv.Handle.t;
  }

  let unwatch w =
    Luv_lwt.in_luv (fun return ->
      Luv.FS_event.stop w.h |> Result.get_ok;
      return ()
    )

  let watch_file path callback =
    Luv_lwt.in_luv (fun return ->
      let h = Luv.FS_event.init () |> Result.get_ok in

      Luv.FS_event.start h path
        (function
        | Ok _ ->
          Luv_lwt.in_lwt_async callback;
        | Error err ->
          Log.err (fun f -> f "watching %s: %s" path (Luv.Error.err_name err)));
      return (Ok { h })
    )

  let%test "watch a file" =
    let filename = Filename.temp_file "vpnkit" "file" in
    let oc = open_out_bin filename in
    Luv_lwt.run begin
      let m = Lwt_mvar.create () in
      watch_file filename (fun () ->
        Lwt.async (fun () -> Lwt_mvar.put m ())
      )
      >>= function
      | Error (`Msg m) ->
        close_out oc;
        Sys.remove filename;
        failwith m
      | Ok w ->
        output_string oc "one";
        flush oc;
        Lwt_mvar.take m
        >>= fun () ->
        output_string oc "two";
        flush oc;
        Lwt_mvar.take m
        >>= fun () ->
        close_out oc;
        Sys.remove filename;
        unwatch w
        >>= fun () ->
        Lwt.return true
    end
end

module Time = struct
  type 'a io = 'a Lwt.t
  let sleep_ns ns =
    Luv_lwt.in_luv (fun return ->
      let timer = Luv.Timer.init () |> Result.get_ok in
      match Luv.Timer.start timer (Duration.to_ms ns) (fun () -> return (Ok ())) with
      | Error err -> return (Error (`Msg (Luv.Error.strerror err)))
      | Ok () -> ()
    ) >>= function
    | Error (`Msg m) -> Lwt.fail_with m
    | Ok () -> Lwt.return_unit

  let%test "Time.sleep_ns wakes up" =
    let start = Unix.gettimeofday () in
    Luv_lwt.run @@ sleep_ns @@ Duration.of_ms 100;
    let duration = Unix.gettimeofday () -. start in
    duration >= 0.1
end


module Dns = struct
  let getaddrinfo node family =
    Luv_lwt.in_luv (fun return ->
      Luv.DNS.getaddrinfo ~family ~node () begin function
      | Error err ->
        return (Error (`Msg (Luv.Error.strerror err)))
      | Ok x ->
        let ips = List.fold_left (fun acc addr_info -> match addr_info.Luv.DNS.Addr_info.family with
        |  `INET ->
          begin match Luv.Sockaddr.to_string addr_info.Luv.DNS.Addr_info.addr with
          | None -> acc
          | Some ip ->
            begin match Ipaddr.of_string ip with
            | Some ip -> ip :: acc
            | None -> acc
            end
          end
        | _ -> acc
        ) [] x in
        return (Ok ips)
      end
    ) >>= function
    | Error (`Msg _) ->
      (* FIXME: error handling completely missing *)
      Lwt.return []
    | Ok ips ->
      Lwt.return ips

  let%test "getaddrinfo dave.recoil.org" =
    Luv_lwt.run begin
      getaddrinfo "dave.recoil.org" `INET
      >>= fun ips ->
      Lwt.return (ips <> [])
    end

  let localhost_local = Dns.Name.of_string "localhost.local"

  let resolve_getaddrinfo question =
    let open Dns.Packet in
    begin match question with
    | { q_class = Q_IN; q_name; _ } when q_name = localhost_local ->
      Log.debug (fun f -> f "DNS lookup of localhost.local: return NXDomain");
      Lwt.return (q_name, [])
    | { q_class = Q_IN; q_type = Q_A; q_name; _ } ->
      getaddrinfo (Dns.Name.to_string q_name) `INET
      >>= fun ips ->
      Lwt.return (q_name, ips)
    | { q_class = Q_IN; q_type = Q_AAAA; q_name; _ } ->
      getaddrinfo (Dns.Name.to_string q_name) `INET6
      >>= fun ips ->
      Lwt.return (q_name, ips)
    | _ ->
      Lwt.return (Dns.Name.of_string "", [])
    end
    >>= function
    | _, [] -> Lwt.return []
    | q_name, ips ->
      let answers = List.map (function
        | Ipaddr.V4 v4 ->
          { name = q_name; cls = RR_IN; flush = false; ttl = 0l; rdata = A v4 }
        | Ipaddr.V6 v6 ->
          { name = q_name; cls = RR_IN; flush = false; ttl = 0l; rdata = AAAA v6 }
        ) ips in
      Lwt.return answers

  let resolve = resolve_getaddrinfo
end

module Main = struct
  let run = Luv_lwt.run
  let%test "Host.Main.Run has a working luv event loop" =
    run begin
      Time.sleep_ns (Duration.of_ms 100)
    end;
    true
  let run_in_main _ = failwith "FIXME: run_in_main not implemented"
end

module Fn = struct
  type ('request, 'response) t = 'request -> 'response
  let create f = f
  let destroy _ = ()
  let fn _ = failwith "FIXME: detach not implemented"
end

let compact () =
  let start = Unix.gettimeofday () in
  Gc.compact();
  let stats = Gc.stat () in
  let time = Unix.gettimeofday () -. start in

  Log.info (fun f -> f
    "Gc.compact took %.1f seconds. Heap has heap_words=%d live_words=%d free_words=%d top_heap_words=%d stack_size=%d"
    time stats.Gc.heap_words stats.Gc.live_words stats.Gc.free_words stats.Gc.top_heap_words stats.Gc.stack_size
  )

let start_background_gc config =
  match config with
  | None ->
    Log.info (fun f -> f "No periodic Gc.compact enabled")
  | Some s ->
    let timer = Luv.Timer.init () |> Result.get_ok in
    Luv.Timer.start timer (5 * 1000) ~repeat:(s * 1000) compact |> Result.get_ok