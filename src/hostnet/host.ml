open Lwt.Infix

let src =
  let src = Logs.Src.create "Luv" ~doc:"Host interface based on Luv" in
  Logs.Src.set_level src (Some Logs.Info);
  src

module Log = (val Logs.src_log src : Logs.LOG)

let default_read_buffer_size = 65536

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

let sockaddr_of_address (dst, dst_port) =
  Unix.ADDR_INET(Unix.inet_addr_of_string @@ Ipaddr.to_string dst, dst_port)

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

  let ip_port_of_sockaddr sockaddr =
    try match sockaddr with
    | Unix.ADDR_INET(ip, port) ->
      Some (Ipaddr.of_string_exn @@ Unix.string_of_inet_addr ip, port)
    | _ -> None
    with _ -> None
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
    let error err =
      let msg =
        Fmt.strf "Socket.Pipe.of_bound_fd failed \
                  with %s" (Luv.Error.strerror err) in
      Log.err (fun f -> f "%s" msg);
      failwith msg in
    Luv.File.open_osfhandle os_fd

  module Datagram = struct
    type address = Ipaddr.t * int

    module Udp = struct
      include Common

      type flow = {
        idx: int option;
        label: string;
        description: string;
        mutable fd: Uwt.Udp.t option;
        read_buffer_size: int;
        mutable already_read: Cstruct.t option;
        sockaddr: Unix.sockaddr;
        address: address;
      }

      type address = Ipaddr.t * int

      let string_of_flow t = Fmt.strf "udp -> %s" (string_of_address t.address)

      let of_fd
          ?idx ?(read_buffer_size = Constants.max_udp_length)
          ?(already_read = None) ~description sockaddr address fd
        =
        let label = match fst address with
        | Ipaddr.V4 _ -> "UDPv4"
        | Ipaddr.V6 _ -> "UDPv6" in
        { idx; label; description; fd = Some fd; read_buffer_size; already_read;
          sockaddr; address }

      let connect ?read_buffer_size address =
        let description = "udp:" ^ (string_of_address address) in
        register_connection description
        >>= fun idx ->
        let label, fd, addr =
          try match fst @@ address with
          | Ipaddr.V4 _ -> "UDPv4", Uwt.Udp.init_ipv4_exn (), Unix.inet_addr_any
          | Ipaddr.V6 _ -> "UDPv6", Uwt.Udp.init_ipv6_exn (), Unix.inet6_addr_any
          with e -> deregister_connection idx; raise e
        in
        Lwt.catch (fun () ->
            let sockaddr = make_sockaddr address in
            let result = Uwt.Udp.bind fd ~addr:(Unix.ADDR_INET(addr, 0)) () in
            if not(Uwt.Int_result.is_ok result) then begin
              let error = Uwt.Int_result.to_error result in
              Log.err (fun f ->
                  f "Socket.%s.connect(%s): %s" label
                    (string_of_address address) (Uwt.strerror error));
              Lwt.fail (Unix.Unix_error(Uwt.to_unix_error error, "bind", ""))
            end else
              Lwt_result.return
                (of_fd ~idx ?read_buffer_size ~description sockaddr address fd)
          ) (fun e ->
            deregister_connection idx;
            log_exception_continue "Udp.connect Uwt.Udp.close_wait"
              (fun () -> Uwt.Udp.close_wait fd)
            >>= fun () ->
            errorf "Socket.%s.connect %s: caught %a" label description Fmt.exn e
          )

      let rec read t = match t.fd, t.already_read with
      | None, _ -> Lwt.return (Ok `Eof)
      | Some _, Some data when Cstruct.len data > 0 ->
        t.already_read <- Some (Cstruct.sub data 0 0); (* next read is `Eof *)
        Lwt.return (Ok (`Data data))
      | Some _, Some _ ->
        Lwt.return (Ok `Eof)
      | Some fd, None ->
        let buf = Cstruct.create t.read_buffer_size in
        Lwt.catch (fun () ->
            Uwt.Udp.recv_ba ~pos:buf.Cstruct.off ~len:buf.Cstruct.len
              ~buf:buf.Cstruct.buffer fd
            >>= fun recv ->
            if recv.Uwt.Udp.is_partial then begin
              Log.err (fun f ->
                  f "Socket.%s.read: dropping partial response (buffer \
                     was %d bytes)" t.label (Cstruct.len buf));
              read t
            end else begin
              let data = `Data (Cstruct.sub buf 0 recv.Uwt.Udp.recv_len) in
              (* Since we're emulating a point-to-point connection, drop any incoming
                 UDP which has the wrong source IP and port. *)
              match recv.Uwt.Udp.sockaddr with
              | Some sockaddr ->
                begin match ip_port_of_sockaddr sockaddr with
                | None ->
                  Log.warn (fun f ->
                    f "Socket.%s.read: packet has invalid source address so \
                       dropping since we're connected to %s" t.label
                      (string_of_address t.address)
                  );
                  read t
                | Some address when address <> t.address ->
                  Log.warn (fun f ->
                    f "Socket.%s.read: dropping response from %s since \
                       we're connected to %s" t.label
                      (string_of_address address)
                      (string_of_address t.address)
                  );
                  read t
                | Some _ ->
                  Lwt.return (Ok data)
                end
              | None ->
                Log.warn (fun f ->
                  f "Socket.%s.read: packet has no source address so \
                     dropping since we're connected to %s" t.label
                    (string_of_address t.address)
                );
                read t
            end
          ) (function
          | Unix.Unix_error(e, _, _) when Uwt.of_unix_error e = Uwt.ECANCELED ->
            (* happens on normal timeout *)
            Lwt.return (Ok `Eof)
          | e ->
            Log.err (fun f ->
                f "Socket.%s.read: %s caught %s returning Eof"
                  t.label
                  (string_of_flow t)
                  (Printexc.to_string e)
              );
            Lwt.return (Ok `Eof)
          )

      let write t buf = match t.fd with
      | None -> Lwt.return (Error `Closed)
      | Some fd ->
        Lwt.catch (fun () ->
            Uwt.Udp.send_ba ~pos:buf.Cstruct.off ~len:buf.Cstruct.len
              ~buf:buf.Cstruct.buffer fd t.sockaddr
            >>= fun () ->
            Lwt.return (Ok ())
          ) (fun e ->
            Log.err (fun f -> f "Socket.%s.write %s: caught %s returning Eof"
                        t.label t.description (Printexc.to_string e));
            Lwt.return (Error `Closed)
          )

      let writev t bufs = write t (Cstruct.concat bufs)

      let close t = match t.fd with
      | None -> Lwt.return_unit
      | Some fd ->
        t.fd <- None;
        Log.debug (fun f -> f "Socket.%s.close: %s" t.label (string_of_flow t));
        log_exception_continue "Udp.close Uwt.Udp.close_wait"
          (fun () -> Uwt.Udp.close_wait fd)
        >|= fun () ->
        match t.idx with Some idx -> deregister_connection idx | None -> ()

      let shutdown_read _t = Lwt.return_unit
      let shutdown_write _t = Lwt.return_unit

      type server = {
        idx: int;
        label: string;
        fd: Uwt.Udp.t;
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
        let description =
          Fmt.strf "udp:%a:%d %s" Ipaddr.pp_hum ip port description
        in
        let sockaddr = make_sockaddr(ip, port) in
        register_connection description >>= fun idx ->
        let fd =
          try Uwt.Udp.init ()
          with e -> deregister_connection idx; raise e
        in
        let result =
          Uwt.Udp.bind ~mode:[ Uwt.Udp.Reuse_addr ] fd ~addr:sockaddr ()
        in
        let label = match ip with
        | Ipaddr.V4 _ -> "UDPv4"
        | Ipaddr.V6 _ -> "UDPv6" in
        let t = make ~idx ~label fd in
        if not(Uwt.Int_result.is_ok result) then begin
          let error = Uwt.Int_result.to_error result in
          Log.debug (fun f ->
              f "Socket.%s.bind(%a, %d): %s" t.label Ipaddr.pp_hum ip port
                (Uwt.strerror error));
          deregister_connection idx;
          Lwt.fail (Unix.Unix_error(Uwt.to_unix_error error, "bind", ""))
        end else Lwt.return t

      let of_bound_fd ?read_buffer_size:_ fd =
        match Uwt.Udp.openudp fd with
        | Uwt.Ok fd ->
          let label, description = match Uwt.Udp.getsockname fd with
          | Uwt.Ok sockaddr ->
            begin match ip_port_of_sockaddr sockaddr with
            | Some (ip, port) ->
              Fmt.strf "udp:%a:%d" Ipaddr.pp_hum ip port,
              begin match ip with
              | Ipaddr.V4 _ -> "UDPv4"
              | Ipaddr.V6 _ -> "UDPv6"
              end
            | _ -> "unknown incoming UDP", "UDP?"
            end
          | Uwt.Error error ->
            "Socket.UDP?.of_bound_fd: getpeername failed: " ^ (Uwt.strerror error),
            "UDP?"
          in
          let idx = register_connection_no_limit description in
          make ~idx ~label fd
        | Uwt.Error error ->
          let msg =
            Fmt.strf "Socket.UDP?.of_bound_fd failed with %s" (Uwt.strerror error)
          in
          Log.err (fun f -> f "Socket.UDP?.of_bound_fd: %s" msg);
          failwith msg

      let getsockname { label; fd; _ } =
        match Uwt.Udp.getsockname_exn fd with
        | Unix.ADDR_INET(iaddr, port) ->
          Ipaddr.of_string_exn (Unix.string_of_inet_addr iaddr), port
        | _ ->
          Fmt.kstrf invalid_arg "Socket.%s.getsockname: passed a non-TCP socket"
            label

      let shutdown server =
        if not server.closed then begin
          server.closed <- true;
          log_exception_continue "Udp.shutdown Uwt.Udp.close_wait"
            (fun () -> Uwt.Udp.close_wait server.fd)
          >>= fun () ->
          deregister_connection server.idx;
          Lwt.return_unit
        end else Lwt.return_unit

      let rec recvfrom server buf =
        Uwt.Udp.recv_ba ~pos:buf.Cstruct.off ~len:buf.Cstruct.len
          ~buf:buf.Cstruct.buffer server.fd
        >>= fun recv ->
        if recv.Uwt.Udp.is_partial then begin
          Log.err (fun f ->
              f "Socket.%s.recvfrom: dropping partial response (buffer was \
                 %d bytes)" server.label (Cstruct.len buf));
          recvfrom server buf
        end else match recv.Uwt.Udp.sockaddr with
        | None ->
          Log.err (fun f ->
              f "Socket.%s.recvfrom: dropping response from unknown sockaddr"
                server.label);
          recvfrom server buf
        | Some sockaddr ->
          begin match ip_port_of_sockaddr sockaddr with
          | Some address -> Lwt.return (recv.Uwt.Udp.recv_len, address)
          | None ->
            Log.err (fun f ->
              f "Socket.%s.recvfrom: dropping response from invalid sockaddr"
                server.label);
            recvfrom server buf
          end

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
              let flow =
                of_fd ~description ~read_buffer_size:0 ~already_read:(Some data)
                  (sockaddr_of_address address) address t.fd
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
            begin match Uwt.Udp.fileno server.fd with
            | Error _ -> ()
            | Ok fd -> Utils.setSocketTTL fd ttl
            end;
            let sockaddr =
              Unix.ADDR_INET(Unix.inet_addr_of_string @@ Ipaddr.to_string ip, port)
            in
            Uwt.Udp.send_ba ~pos:buf.Cstruct.off ~len:buf.Cstruct.len
              ~buf:buf.Cstruct.buffer server.fd sockaddr
          )
    end

  end

  module Stream = struct

    (* Common across TCP and Pipes *)

    let read_into fd buf =
      let buffer = Luv.Buffer.sub buf.Cstruct.buffer ~offset:buf.Cstruct.off ~length:buf.Cstruct.len in
      let result, u = Luv_lwt.task () in
      Luv.Stream.read_start ~allocate:(fun _suggested -> buffer) fd begin function
        | Ok b ->
          let n = Luv.Buffer.size buffer - (Luv.Buffer.size b) in
          if n == 0 then begin
            match Luv.Stream.read_stop fd with
            | Ok () -> Luv_lwt.wakeup_later u (Ok (`Data ()))
            | Error err -> Luv_lwt.wakeup_later u (Error (`Msg (Luv.Error.strerror err)))
          end
        | Error err ->
          Luv_lwt.wakeup_later u (Error (`Msg (Luv.Error.strerror err)))
        end;
      result

    let read fd =
      let result, u = Luv_lwt.task () in
      Luv.Stream.read_start fd begin function
      | Ok buf ->
        begin match Luv.Stream.read_stop fd with
        | Ok () -> Luv_lwt.wakeup_later u (Ok (`Data (Cstruct.of_bigarray buf)))
        | Error err -> Luv_lwt.wakeup_later u (Error (`Msg (Luv.Error.strerror err)))
        end
      | Error err ->
        Luv_lwt.wakeup_later u (Error (`Msg (Luv.Error.strerror err)))
      end;
      result

    let writev fd bufs =
      let buffers = List.map (fun buf -> Luv.Buffer.sub buf.Cstruct.buffer ~offset:buf.Cstruct.off ~length:buf.Cstruct.len) bufs in
      let result, u = Luv_lwt.task () in
      let rec loop buffers =
        if Luv.Buffer.total_size buffers == 0
        then Luv_lwt.wakeup_later u (Ok ())
        else Luv.Stream.write fd buffers begin fun r n -> match r with
        | Error err ->
          Luv_lwt.wakeup_later u (Error (`Msg (Luv.Error.strerror err)))
        | Ok () ->
          loop (Luv.Buffer.drop buffers n)
        end in
      loop buffers;
      result


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

      let connect ?(read_buffer_size = default_read_buffer_size) (ip, port) =
        let description = Fmt.strf "tcp:%a:%d" Ipaddr.pp_hum ip port in
        let label = match ip with
        | Ipaddr.V4 _ -> "TCPv4"
        | Ipaddr.V6 _ -> "TCPv6" in

        register_connection_noexn description
        >>= function
        | None ->
          errorf "Socket.%s.connect %s: hit connection limit" label description
        | Some idx ->
          let return_error err =
            deregister_connection idx;
            let msg = Fmt.strf "Socket.%s.connect %s:%d: %s" label (Ipaddr.to_string ip) port (Luv.Error.strerror err) in
            Log.err (fun f -> f "%s" msg);
            Lwt.return (Error (`Msg msg)) in
          begin match Luv.TCP.init () with
          | Error err -> return_error err
          | Ok fd ->
            begin match make_sockaddr (ip, port) with
            | Error err ->
              Luv.Handle.close fd ignore;
              return_error err
            | Ok sockaddr ->
              let t, u = Luv_lwt.task () in
              Luv.TCP.connect fd sockaddr (Luv_lwt.wakeup_later u);
              t >>= function
              | Error err ->
                Luv.Handle.close fd ignore;
                return_error err
              | Ok () ->
                Lwt.return (Ok (of_fd ~description ~idx ~label fd))
            end
          end

      let shutdown_read _ =
        Lwt.return ()

      let shutdown_write { label; description; fd; closed; _ } =
        if not closed then Luv.Stream.shutdown fd begin function
          | Error err -> Log.err (fun f -> f "Socket.%s.shutdown_write: %s" label (Luv.Error.strerror err))
          | Ok () -> ()
        end;
        Lwt.return_unit

      let read_into t buf = read_into t.fd buf
      let read t = read t.fd
      let writev t bufs = writev t.fd bufs
      let write t buf = writev t [ buf ]

      let close t =
        if not t.closed then begin
          t.closed <- true;
          Luv.Handle.close t.fd ignore;
          deregister_connection t.idx;
        end;
        Lwt.return_unit

      type server = {
        label: string;
        mutable listening_fds: (int * Luv.TCP.t) list;
        mutable disable_connection_tracking: bool;
      }

      let getsockname' fd = match Luv.TCP.getsockname fd with
        | Error err -> invalid_arg "Tcp.getsockname passed a non-TCP socket"
        | Ok sockaddr ->
          begin match parse_sockaddr sockaddr with
          | Error _ -> invalid_arg "Tcp.getsockname unable to parse Sockaddr.t"
          | Ok (ip, port) -> ip, port
          end

      let label_of ip = match ip with
        | Ipaddr.V4 _, _ -> "TCPv4"
        | Ipaddr.V6 _, _ -> "TCPv6"

      let make ?(read_buffer_size = default_read_buffer_size) listening_fds =
        let label = match listening_fds with
        | [] -> failwith "socket is closed"
        | (_, fd) :: _ -> label_of @@ fst @@ getsockname fd in
        { label; listening_fds;
          disable_connection_tracking = false }

      let disable_connection_tracking server =
        server.disable_connection_tracking <- true

      let getsockname server = match server.listening_fds with
        | [] -> failwith "socket is closed"
        | (_, fd) :: _ -> getsockname' fd

      let bind_one ?(description="") (ip, port) =
        let label = match ip with
        | Ipaddr.V4 _ -> "TCPv4"
        | Ipaddr.V6 _ -> "TCPv6" in
        let description =
          Fmt.strf "tcp:%a:%d %s" Ipaddr.pp_hum ip port description
        in
        register_connection description >>= fun idx ->
        let return_error err =
          deregister_connection idx;
          let msg = Fmt.strf "Socket.%s.bind_one %s:%d: %s" label (Ipaddr.to_string ip) port (Luv.Error.strerror err) in
          Log.err (fun f -> f "%s" msg);
          Lwt.return (Error (`Msg msg)) in
        match Luv.TCP.init() with
        | Error err -> return_error err
        | Ok fd ->
          begin match make_sockaddr (ip, port) with
          | Error err -> return_error err
          | Ok sockaddr ->
            begin match Luv.TCP.bind fd sockaddr with
            | Error err -> return_error err
            | Ok () ->
              (* Determine the local port number if the user requested INADDR_ANY *)
              begin match Luv.TCP.getsockname fd with
              | Error err -> return_error err
              | Ok sockaddr ->
                begin match parse_sockaddr sockaddr with
                | Error err -> return_error err
                | Ok (_, port) -> Lwt.return (idx, label, fd, port)
                end
              end
            end
          end

      let bind ?description (ip, port) =
        bind_one ?description (ip, port)
        >>= fun (idx, label, fd, local_port) ->
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
              >>= fun (idx, _, fd) ->
              Lwt.return [ idx, fd ]
            end else
              Lwt.return []
          ) (fun e ->
            Log.debug (fun f ->
                f "Ignoring failed bind to ::1:%d (%a)" local_port Fmt.exn e);
            Lwt.return []
          )
        >|= fun extra ->
        make ((idx, fd) :: extra)

      let shutdown server =
        let fds = server.listening_fds in
        server.listening_fds <- [];
        Lwt_list.iter_s (fun (idx, fd) ->
          let t, u = Luv_lwt.task () in
          Luv.Handle.close fd (Luv_lwt.wakeup_later u);
          t >>= fun () ->
          deregister_connection idx;
          Lwt.return_unit
        ) fds

      let of_bound_fd ?(read_buffer_size = default_read_buffer_size) fd =
        match file_of_file_descr fd with
        | Error err -> error err
        | Ok file ->
          let fd = Luv.TCP.init () |> Result.get_ok in
          begin match Luv.TCP.open_ fd file with
          | Error err -> error err
          | Ok () ->
            let description = match Luv.TCP.getsockname fd with
              | Ok sockaddr ->
                begin match parse_sockaddr sockaddr with
                | Error _ -> "unable to parse sockaddr"
                | Ok (ip, port) -> Fmt.strf "tcp:%s:%d" (Ip.to_string ip) port
                end
              | Error err -> "getsockname failed: " ^ (Luv.Error.strerror err) in
            let idx = register_connection_no_limit description in
            make ~read_buffer_size [ (idx, fd) ]
          end

      let accept_queue = Luv_lwt.make_queue (fun (cb, pipe) -> Lwt.async (cb pipe))

      let listen server' cb =
        let ip, port = getsockname server' in
        let label = label_of ip in
        let description = Fmt.strf "%s:%s:%d" label (Ipaddr.to_string ip) port in
        let handle_connection client () =
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
                      Luv_lwt.push accept_queue (handle_connection, client);
                      accept_forever ()
                    end
                  end
                end in
            accept_forever ()
          end
        ) server'.listening_fds
    end

    module Unix = struct
      include Common

      type address = string

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
        begin match Luv.Pipe.init () with
        | Error err ->
          deregister_connection idx;
          let msg = Fmt.strf "Pipe.connect %s: %s" path (Luv.Error.strerror err) in
          Log.err (fun f -> f "%s" msg);
          Lwt.fail_with msg
        | Ok fd ->
          let t, u = Luv_lwt.task () in
          Luv.Pipe.connect fd path begin function
          | Error err ->
            deregister_connection idx;
            Luv_lwt.wakeup_later u (Error (`Msg (Luv.Error.strerror err)))
          | Ok () -> Luv_lwt.wakeup_later u (Ok (of_fd ~description ~idx fd))
          end;
          t
        end

      let shutdown_read _ =
        Lwt.return ()

      let shutdown_write { fd; closed; _ } =
        if not closed then Luv.Stream.shutdown fd begin function
          | Error err -> Log.err (fun f -> f "Pipe.shutdown_write: %s" (Luv.Error.strerror err))
          | Ok () -> ()
        end;
        Lwt.return_unit

      let read_into t buf = read_into t.fd buf
      let read t = read t.fd
      let writev t bufs = writev t.fd bufs
      let write t buf = writev t [ buf ]

      let close t =
        if not t.closed then begin
          t.closed <- true;
          Luv.Handle.close t.fd ignore;
          deregister_connection t.idx;
        end;
        Lwt.return_unit

      type server = {
        idx: int;
        fd: Luv.Pipe.t;
        mutable closed: bool;
        mutable disable_connection_tracking: bool;
      }

      let bind ?(description="") path =
        Lwt.catch (fun () -> Lwt_unix.unlink path) (fun _ -> Lwt.return ())
        >>= fun () ->
        let description = Fmt.strf "unix:%s %s" path description in
        register_connection description >>= fun idx ->
        match Luv.Pipe.init () with
        | Error err -> Lwt.fail (Luv_lwt.Error err)
        | Ok fd ->
          begin match Luv.Pipe.bind fd path with
          | Ok () ->
            Lwt.return { idx; fd; closed = false;
                        disable_connection_tracking = false }
          | Error err ->
            deregister_connection idx;
            Lwt.fail (Luv_lwt.Error err)
          end

      let getsockname server = match Luv.Pipe.getsockname server.fd with
      | Ok path -> path
      | _ -> invalid_arg "Unix.sockname passed a non-Unix socket"

      let disable_connection_tracking server =
        server.disable_connection_tracking <- true

      let accept_queue = Luv_lwt.make_queue (fun (cb, pipe) -> Lwt.async (cb pipe))

      let listen ({ fd; _ } as server') cb =
        let handle_connection client () =
          let description = "unix:" ^ getsockname server' in
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
                Luv_lwt.push accept_queue (handle_connection, client);
                accept_forever ()
              end in
          accept_forever ()
        end

      let of_bound_fd ?(read_buffer_size = default_read_buffer_size) fd =
        match file_of_file_descr fd with
        | Error err -> error err
        | Ok file ->
          let fd = Luv.Pipe.init () |> Result.get_ok in
          begin match Luv.Pipe.open_ fd file with
          | Error err -> error err
          | Ok () ->
            let description = match Luv.Pipe.getsockname fd with
              | Ok path -> "unix:" ^ path
              | Error err -> "getsockname failed: " ^ (Luv.Error.strerror err) in
              let idx = register_connection_no_limit description in
              { idx; fd; closed = false; disable_connection_tracking = false }
          end

      let shutdown server =
        if not server.closed then begin
          server.closed <- true;
          let t, u = Luv_lwt.task () in
          Luv.Handle.close server.fd (Luv_lwt.wakeup_later u);
          t >>= fun () ->
          deregister_connection server.idx;
          Lwt.return_unit
        end else
          Lwt.return_unit
    end
  end
end

module Files = struct
  let read_file path =
    let t, u = Luv_lwt.task () in

    let close h = Luv.File.close h
      (function
      | Ok () -> ()
      | Error err -> Log.err (fun f -> f "closing: %s" (Luv.Error.err_name err))) in

    (* Caller wants a string *)
    let buf = Buffer.create 4096 in
    let frag = Luv.Buffer.create 4096 in
    let rec loop h =
      Luv.File.read h [ frag ]
        (function
          | Ok n ->
            if n = Unsigned.Size_t.zero then begin
              close h;
              Luv_lwt.wakeup_later u (Ok (Buffer.contents buf))
            end else begin
              Luv.Buffer.(sub frag ~offset:0 ~length:(Unsigned.Size_t.to_int n) |> to_bytes) |> Buffer.add_bytes buf;
              loop h
            end
          | Error err ->
            close h;
            Luv_lwt.wakeup_later u (Error (`Msg (Luv.Error.strerror err)))
        ) in
    Luv.File.open_ path [ `RDONLY ]
      (function
      | Error err ->
        Luv_lwt.wakeup_later u (Error (`Msg (Luv.Error.strerror err)))
      | Ok h ->
        loop h
      );
    t

  type watch = {
    h: [ `FS_event ] Luv.Handle.t;
    n: int;
  }

  let unwatch w =
    Luv.FS_event.stop w.h |> Result.get_ok;
    Lwt_unix.stop_notification w.n

  let watch_file path callback =
    let h = Luv.FS_event.init () |> Result.get_ok in
    let n = Lwt_unix.make_notification callback in

    Luv.FS_event.start h path
      (function
      | Ok _ ->
        Lwt_unix.send_notification n
      | Error err ->
        Log.err (fun f -> f "watching %s: %s" path (Luv.Error.err_name err)));
    Ok { h; n }

end

module Time = struct
  type 'a io = 'a Lwt.t
  let sleep_ns ns =
      let timer = Luv.Timer.init () |> Result.get_ok in
      let t, u = Luv_lwt.task () in
      ignore (Luv.Timer.start timer (Duration.to_ms ns) (Luv_lwt.wakeup_later u));
      t
end

module Dns = struct
  (* FIXME: error handling completely missing *)
  let getaddrinfo node family =
    let t, u = Luv_lwt.task () in
    Luv.DNS.getaddrinfo ~family ~node () (Luv_lwt.wakeup_later u);
    t >>= function
    | Error _ ->
      Lwt.return []
    | Ok x ->
      Lwt.return @@
      List.fold_left (fun acc addr_info -> match addr_info.Luv.DNS.Addr_info.family with
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
        ) [] x

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
  let run = Uwt.Main.run
  let run_in_main = Uwt_preemptive.run_in_main
end

module Fn = struct
  type ('request, 'response) t = 'request -> 'response
  let create f = f
  let destroy _ = ()
  let fn = Uwt_preemptive.detach
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
  let () = match config with
  | None ->
    Log.info (fun f -> f "No periodic Gc.compact enabled")
  | Some s ->
    let rec loop () =
      Time.sleep_ns (Duration.of_sec s)
      >>= fun () ->
      compact ();
      loop () in
    Lwt.async loop
  in
  if Sys.os_type = "Unix" then begin
    (* This fails with EINVAL on Windows *)
    let (_: Uwt.Signal.t) = Uwt.Signal.start_exn Sys.sigusr1 ~cb:(fun _t _signal ->
        Log.info (fun f -> f "Received SIGUSR1");
        compact ()
    ) in
    ()
  end