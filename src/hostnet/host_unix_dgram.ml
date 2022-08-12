type 'a request = {
  buf: Cstruct.t;
  done_u: 'a Lwt.u;
}

type 'a one_direction = {
  mutable request: 'a request option;
  mutable closed: bool;
  c: Condition.t;
  m: Mutex.t;
}

let push d buf =
  let done_t, done_u = Lwt.task () in
  let r = { buf; done_u } in
  Mutex.lock d.m;
  match d.closed, d.request with
  | true, _ ->
    Lwt.wakeup_exn done_u (Failure "socket is closed");
    Mutex.unlock d.m;
    done_t
  | false, Some _ ->
    (* Expect at-most-one in progress operation at a time *)
    Lwt.wakeup_exn done_u (Failure "request already in progress");
    Mutex.unlock d.m;
    done_t
  | false, None ->
    d.request <- Some r;
    Condition.signal d.c;
    Mutex.unlock d.m;
    done_t

let worker f d =
  Thread.create (fun () ->
    Mutex.lock d.m;
    let rec loop () =
      while not d.closed && d.request = None do
        Condition.wait d.c d.m;
      done;
      begin match d.request with
      | None -> ()
      | Some r ->
        d.request <- None;
        try
          let result = f r in
          Luv_lwt.in_lwt_async (fun () -> Lwt.wakeup_later r.done_u result)
        with e ->
          Luv_lwt.in_lwt_async (fun () -> Lwt.wakeup_exn r.done_u e)
      end;
      if not d.closed then loop () in
    loop ()
  ) ()

let close d =
  Mutex.lock d.m;
  d.closed <- true;
  Condition.signal d.c;
  Mutex.unlock d.m

let make_one_direction f =
  let d = {
    request = None;
    closed = false;
    c = Condition.create();
    m = Mutex.create();
  } in
  let (_: Thread.t) = worker f d in
  d

type flow = {
  fd: Unix.file_descr;
  send: int one_direction;
  recv: int one_direction;
  mtu: int;
}

let of_bound_fd ?(mtu=1500) fd =
  let send_buffer = Bytes.create mtu in
  let send = make_one_direction (fun request ->
    let len = min (Cstruct.length request.buf) (Bytes.length send_buffer) in
    Cstruct.blit_to_bytes request.buf 0 send_buffer 0 len;
    Unix.send fd send_buffer 0 len []
  ) in
  let recv_buffer = Bytes.create mtu in
  let recv = make_one_direction (fun request ->
    let len = min (Cstruct.length request.buf) (Bytes.length send_buffer) in
    let n = Unix.recv fd recv_buffer 0 len [] in
    Cstruct.blit_from_bytes recv_buffer 0 request.buf 0 n;
    n
  ) in
  Lwt.return {fd; send; recv; mtu}

let send flow buf =
  Lwt.catch
    (fun () -> push flow.send buf)
    (function
      | Unix.Unix_error(Unix.ENOBUFS, _, _) ->
        (* If we're out of buffer space we have to drop the packet *)
        Lwt.return 0
      | e ->
        Lwt.fail e)

let recv flow buf = push flow.recv buf

let close flow =
  close flow.send;
  close flow.recv;
  Unix.close flow.fd

let%test_unit "socketpair" =
  if Sys.os_type <> "Win32" then begin
    let a, b = Unix.socketpair Unix.PF_UNIX Unix.SOCK_DGRAM 0 in
    Lwt_main.run begin
      let open Lwt.Infix in
      of_bound_fd a
      >>= fun a_flow ->
      of_bound_fd b
      >>= fun b_flow ->
      let rec loop () =
        Lwt.catch (fun () ->
          send a_flow (Cstruct.of_string "hello")
          >>= fun n ->
          Lwt.return (Some n)
        ) (function
          | Unix.Unix_error(Unix.ENOTCONN, _, _) -> Lwt.return None
          | e -> Lwt.fail e)
        >>= function
        | None -> Lwt.return_unit
        | Some n ->
          if n <> 5 then failwith (Printf.sprintf "send returned %d, expected 5" n);
          Lwt_unix.sleep 1.
          >>= fun () ->
          loop () in
      let _ = loop () in
      let buf = Cstruct.create 1024 in
      recv b_flow buf
      >>= fun n ->
      if n <> 5 then failwith (Printf.sprintf "recv returned %d, expected 5" n);
      let received = Cstruct.(to_string (sub buf 0 n)) in
      if received <> "hello" then failwith (Printf.sprintf "recv returned '%s', expected 'hello'" received);
      Printf.fprintf stderr "closing\n";
      close a_flow;
      close b_flow;
      Lwt.return_unit
    end
  end

type error = [ `Closed ]

let pp_error ppf `Closed = Fmt.string ppf "Closed"

type write_error = error

let pp_write_error = pp_error

open Lwt.Infix

let read t =
  let buf = Cstruct.create t.mtu in
  recv t buf
  >>= fun n ->
  if n = 0
  then Lwt.return @@ Ok `Eof
  else Lwt.return @@ Ok (`Data (Cstruct.sub buf 0 n))

let read_into t buf =
  (* FIXME: this API doesn't work with datagrams *)
  recv t buf
  >>= fun n ->
  if n <> Cstruct.length buf
  then failwith (Printf.sprintf "read_into buf len = %d, only read %d" (Cstruct.length buf) n)
  else Lwt.return @@ Ok (`Data ())

let write t buf =
  send t buf
  >>= fun n ->
  if n <> Cstruct.length buf
  then failwith (Printf.sprintf "send buf len = %d, only sent %d" (Cstruct.length buf) n)
  else Lwt.return @@ Ok ()

let writev t bufs =
  let buf = Cstruct.concat bufs in
  write t buf

let close t =  close t; Lwt.return_unit

type server = {
  fd: Unix.file_descr;
}

type address = string

let magic = "VMNET"
let error_message = "This socket receives SOCK_DGRAM file descriptors for sending and receiving ethernet frames.\nIt cannot be used directly.\n"
let success_message = "OK"

let connect address =
  let fd_t, fd_u = Lwt.task () in
  let _ : Thread.t = Thread.create (fun () ->
    let s = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    Unix.connect s (Unix.ADDR_UNIX address);
    let a, b = Unix.socketpair Unix.PF_UNIX Unix.SOCK_DGRAM 0 in
    let _ : int = Fd_send_recv.send_fd s (Bytes.of_string magic) 0 (String.length magic) [] a in
    let buf = Bytes.create (String.length error_message) in
    let n = Unix.read s buf 0 (Bytes.length buf) in
    Unix.close s;
    let response = Bytes.sub buf 0 n |> Bytes.to_string in
    if response <> success_message
    then failwith ("Host_unix_dgram.connect: " ^ response);
    Luv_lwt.in_lwt_async (fun () -> Lwt.wakeup_later fd_u b)
  ) () in
  let open Lwt.Infix in
  fd_t >>= fun fd ->
  of_bound_fd fd

let bind ?description:_ address =
  let fd_t, fd_u = Lwt.task () in
  let _ : Thread.t = Thread.create (fun () ->
    let result =
      try
        let s = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
        Unix.bind s (Unix.ADDR_UNIX address);
        Unix.listen s 5;
        Ok s
      with e -> Error e in
      Luv_lwt.in_lwt_async (fun () -> Lwt.wakeup_later fd_u result)
  ) () in
  let open Lwt.Infix in
  fd_t >>= function
  | Ok fd -> Lwt.return { fd }
  | Error e -> Lwt.fail e

let listen server cb =
  let _ : Thread.t = Thread.create (fun () ->
    while true do
      let fd, _ = Unix.accept server.fd in
      let result = Bytes.make 8 '\000' in
      let n, _, received_fd = Fd_send_recv.recv_fd fd result 0 (Bytes.length result) [] in
      let actual_magic = Bytes.sub result 0 n |> Bytes.to_string in
      if actual_magic <> magic then begin
        (* Probably someone accidentally connected to this socket to see what it was. *)
        let m = Bytes.of_string error_message in
        let _ : int = Unix.write fd m 0 (Bytes.length m) in
        Unix.close fd;
      end else begin
        let m = Bytes.of_string success_message in
        let _ : int = Unix.write fd m 0 (Bytes.length m) in
        Unix.close fd;
        Luv_lwt.in_lwt_async (fun () ->
          Lwt.async (fun () ->
            of_bound_fd received_fd
            >>= fun flow ->
            cb flow
          )
        )
      end
    done
  ) () in
  ()

let shutdown server =
  let done_t, done_u = Lwt.task () in
  let _ : Thread.t = Thread.create (fun () ->
    Unix.close server.fd;
    Luv_lwt.in_lwt_async (fun () -> Lwt.wakeup_later done_u ())
  ) () in
  done_t

let%test_unit "host_unix_dgram" =
  if Sys.os_type <> "Win32" then begin
    Lwt_main.run begin
      let address = "/tmp/host_unix_dgram.sock" in
      bind address
      >>= fun server ->
      listen server
        (fun flow ->
          let buf = Cstruct.create 1024 in
          recv flow buf
          >>= fun n ->
          send flow (Cstruct.sub buf 0 n)
          >>= fun m ->
          if n <> m then failwith (Printf.sprintf "n (%d) <> m (%d)" n m);
          Lwt.return_unit
        );
      connect address
      >>= fun flow ->
      let message = "hello" in
      let buf = Cstruct.create (String.length message) in
      Cstruct.blit_from_string message 0 buf 0 (String.length message);
      send flow buf
      >>= fun n ->
      if n <> (String.length message)
      then failwith (Printf.sprintf "n (%d) <> String.length message (%d)" n (String.length message));
      let buf = Cstruct.create (String.length message) in
      recv flow buf
      >>= fun n ->
      if n <> (String.length message)
      then failwith (Printf.sprintf "n (%d) <> String.length message (%d)" n (String.length message));
      let response = Cstruct.to_string buf in
      if message <> response
      then failwith (Printf.sprintf "message (%s) <> response (%s)" message response);
      close flow
    end
  end