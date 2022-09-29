let src =
  let src = Logs.Src.create "Datagram" ~doc:"Host SOCK_DGRAM implementation" in
  Logs.Src.set_level src (Some Logs.Info);
  src

module Log = (val Logs.src_log src : Logs.LOG)

type flow = {
  (* SOCK_DGRAM socket. Ethernet frames are sent and received using send(2) and recv(2) *)
  fd: Unix.file_descr;
  (* A transmit queue. Packets are transmitted asynchronously by a background thread. *)
  send_q: Cstruct.t Queue.t;
  mutable send_done: bool;
  send_m: Mutex.t;
  send_c: Condition.t;
  (* A receive queue. Packets are received asynchronously by a background thread. *)
  recv_q: Cstruct.t Queue.t;
  (* Amount of data currently queued *)
  mutable recv_len: int;
  recv_m: Mutex.t;
  (* Signalled when there is space in the queue *)
  recv_c: Condition.t;
  (* If the receive queue is empty then an Lwt thread can block itself here and will be woken up
     by the next packet arrival. If there is no waiting Lwt thread then packets are queued. *)
  mutable recv_u: Cstruct.t Lwt.u option;
  mtu: int;
}

let max_buffer = Constants.mib

exception Done

let of_bound_fd ?(mtu=65536) fd =
  Log.info (fun f -> f "SOCK_DGRAM interface using MTU %d" mtu);
  let send_q = Queue.create () in
  let send_done = false in
  let send_m = Mutex.create () in
  let send_c = Condition.create () in
  let recv_q = Queue.create () in
  let recv_len = 0 in
  let recv_m = Mutex.create () in
  let recv_c = Condition.create () in
  let recv_u = None in
  let t = {fd; send_q; send_done; send_m; send_c; recv_q; recv_len; recv_m; recv_c; recv_u; mtu} in

  let _ : Thread.t = Thread.create (fun () ->
    (* Background transmit thread *)
    try
      while true do
        Mutex.lock send_m;
        while Queue.is_empty send_q && not t.send_done do
          Condition.wait send_c send_m;
        done;
        if t.send_done then raise Done;
        let to_send = Queue.copy send_q in
        Queue.clear send_q;
        Mutex.unlock send_m;
        Queue.iter (fun packet ->
          try
            let n = Utils.cstruct_send fd packet in
            Log.debug (fun f -> f "send %d" n);
            let len = Cstruct.length packet in
            if n <> len
            then Log.warn (fun f -> f "Utils.cstruct_send packet length %d but sent only %d" len n)
          with Unix.Unix_error(Unix.ENOBUFS, _, _) ->
            (* If we're out of buffer space we have to drop the packet *)
            Log.warn (fun f -> f "ENOBUFS: dropping packet")
        ) to_send;
      done
    with
    | Unix.Unix_error(Unix.EBADF, _, _) ->
      Log.info (fun f -> f "send: EBADFD: connection has been closed, stopping thread")
    | Done ->
      Log.info (fun f -> f "send: fd has been closed, stopping thread")
    | Unix.Unix_error(Unix.ECONNREFUSED, _, _) ->
      Log.info (fun f -> f "send: ECONNREFUSED: stopping thread")
  ) () in

  let _: Thread.t = Thread.create (fun() ->
    try
      (* Many packets are small ACKs so cache an allocated buffer *)
      let allocation_size = Constants.mib in
      let recv_buffer = ref (Cstruct.create allocation_size) in
      while true do
        if Cstruct.length !recv_buffer < mtu
        then recv_buffer := Cstruct.create allocation_size;
        let n = Utils.cstruct_recv fd !recv_buffer in
        let packet = Cstruct.sub !recv_buffer 0 n in
        recv_buffer := Cstruct.shift !recv_buffer n;
        Log.debug (fun f -> f "recv %d" n);
        Mutex.lock recv_m;
        let handled = ref false in
        while not !handled do
          match t.recv_u with
          | None ->
            (* No-one is waiting so consider queueing the packet *)
            if n + t.recv_len > max_buffer then begin
              Condition.wait t.recv_c t.recv_m
              (* Note we need to check t.recv_u again *)
            end else begin
              Queue.push packet recv_q;
              t.recv_len <- t.recv_len + n;
              handled := true;
            end
          | Some waiter ->
            (* A caller is blocked in recv already *)
            Luv_lwt.in_lwt_async (fun () -> Lwt.wakeup_later waiter packet);
            t.recv_u <- None;
            handled := true;
        done;
        (* Is someone already waiting *)
        Mutex.unlock recv_m;
      done
    with
    | Unix.Unix_error(Unix.EBADF, _, _) ->
      Log.info (fun f -> f "recv: EBADFD: connection has been closed, stopping thread")
    | Unix.Unix_error(Unix.ECONNREFUSED, _, _) ->
      Log.info (fun f -> f "recv: ECONNREFUSED: stopping thread")
  ) () in
  Lwt.return t

let send flow buf =
  (* Since we don't wait to send the buffer we have to copy it *)
  let len = Cstruct.length buf in
  let copy = Cstruct.create len in
  Cstruct.blit buf 0 copy 0 len;
  Mutex.lock flow.send_m;
  Queue.push copy flow.send_q;
  Condition.signal flow.send_c;
  Mutex.unlock flow.send_m;
  Lwt.return len

let recv flow =
  Mutex.lock flow.recv_m;
  if not(Queue.is_empty flow.recv_q) then begin
    (* A packet is already queued *)
    let packet = Queue.pop flow.recv_q in
    flow.recv_len <- flow.recv_len - (Cstruct.length packet);
    Condition.signal flow.recv_c;
    Mutex.unlock flow.recv_m;
    Lwt.return packet
  end else begin
    (* The TCP stack should only call recv serially, otherwise packets will be permuted *)
    assert (flow.recv_u = None);
    let t, u = Lwt.wait () in
    flow.recv_u <- Some u;
    Condition.signal flow.recv_c;
    Mutex.unlock flow.recv_m;
    (* Wait for a packet to arrive *)
    t
  end

let close flow =
  Mutex.lock flow.send_m;
  flow.send_done <- true;
  Condition.signal flow.send_c;
  Mutex.unlock flow.send_m;
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
      recv b_flow
      >>= fun buf ->
      let n = Cstruct.length buf in
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
  recv t
  >>= fun buf ->
  let n = Cstruct.length buf in
  if n = 0
  then Lwt.return @@ Ok `Eof
  else Lwt.return @@ Ok (`Data (Cstruct.sub buf 0 n))

let read_into _t _buf = failwith "read_into not implemented for SOCK_DGRAM"
(*
let read_into t buf =
  (* FIXME: this API doesn't work with datagrams *)
  recv t
  >>= fun buf' ->
  let n = Cstruct.length buf' in
  if n <> Cstruct.length buf
  then failwith (Printf.sprintf "read_into buf len = %d, only read %d" (Cstruct.length buf) n)
  else
    Cstruct.blit buf' 0 buf 0 
    Lwt.return @@ Ok (`Data ())
*)
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
    let result =
      try
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
        Ok b
      with e -> Error e in
    Luv_lwt.in_lwt_async (fun () -> Lwt.wakeup_later fd_u result)
  ) () in
  let open Lwt.Infix in
  fd_t >>= function
  | Ok fd -> of_bound_fd fd
  | Error e -> Lwt.fail e

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
      (try Unix.unlink address with Unix.Unix_error(Unix.ENOENT, _, _) -> ());
      bind address
      >>= fun server ->
      listen server
        (fun flow ->
          recv flow
          >>= fun buf ->
          let n = Cstruct.length buf in
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
      recv flow
      >>= fun buf ->
      let n = Cstruct.length buf in
      if n <> (String.length message)
      then failwith (Printf.sprintf "n (%d) <> String.length message (%d)" n (String.length message));
      let response = Cstruct.to_string buf in
      if message <> response
      then failwith (Printf.sprintf "message (%s) <> response (%s)" message response);
      close flow
    end
  end