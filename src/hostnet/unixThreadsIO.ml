let src =
  let src = Logs.Src.create "directio" ~doc:"Host Unix I/O with threads" in
  Logs.Src.set_level src (Some Logs.Info);
  src

module Log = (val Logs.src_log src : Logs.LOG)

module type SendRecv = sig
  val send : Unix.file_descr -> Cstructs.t -> int
  val recv : Unix.file_descr -> Cstruct.t -> int
end

module Make (SR : SendRecv) = struct
  type 'a one_direction = {
    mutable exit_status : exn option;  (** exit status of the thread *)
    q : Cstruct.t Queue.t;  (** data to send *)
    mutable len : int;  (** length of data queued *)
    mutable shutdown_request : bool;  (** half-close requested *)
    waiters : 'a Lwt.u Queue.t;
        (** callers waiting for something; readers wait a buffer whereas writers want transmission confirmation *)
    m : Mutex.t;
    c : Condition.t;
  }
  (** Each direction of a bi-directional flow serviced by a thread *)

  let make_one_direction () =
    {
      exit_status = None;
      q = Queue.create ();
      len = 0;
      shutdown_request = false;
      waiters = Queue.create ();
      m = Mutex.create ();
      c = Condition.create ();
    }

  type flow = {
    fd : Unix.file_descr;
    mutable closed : bool;  (** prevent double-closes *)
    closed_m : Mutex.t;
    send : unit one_direction;
    send_t : Thread.t;
    recv : Cstruct.t one_direction;
    recv_t : Thread.t;
  }

  let max_buffer = Constants.mib

  exception Done

  let finally f g =
    try
      let result = f () in
      g ();
      result
    with e ->
      g ();
      raise e

  let with_lock m f =
    Mutex.lock m;
    finally f (fun () -> Mutex.unlock m)

  let send_exit send e =
    with_lock send.m (fun () ->
        send.exit_status <- Some e;
        Condition.signal send.c;
        Queue.clear send.q;
        send.len <- 0;
        let to_wake = Queue.copy send.waiters in
        Queue.clear send.waiters;
        Luv_lwt.in_lwt_async (fun () ->
            (* Wake up all blocked calls to send *)
            Queue.iter (fun u -> Lwt.wakeup_later u ()) to_wake))

  let send_thread fd send =
    try
      while true do
        let action =
          with_lock send.m (fun () ->
              while Queue.is_empty send.q && not send.shutdown_request do
                Condition.wait send.c send.m
              done;
              let bufs =
                Queue.fold (fun xs x -> x :: xs) [] send.q |> List.rev
              in
              Queue.clear send.q;
              send.len <- 0;
              let to_wake = Queue.copy send.waiters in
              Queue.clear send.waiters;
              Luv_lwt.in_lwt_async (fun () ->
                  (* Wake up all blocked calls to send *)
                  Queue.iter (fun u -> Lwt.wakeup_later u ()) to_wake);
              (* Always try to write buffered data before we quit *)
              if Cstructs.len bufs = 0 && send.shutdown_request then `Done
              else `Write bufs)
        in
        match action with
        | `Done -> raise Done
        | `Write bufs ->
            let n = SR.send fd bufs in
            let len = Cstructs.len bufs in
            if n <> len then
              Log.warn (fun f ->
                  f "writev buffer length %d but sent only %d" len n)
      done
    with
    | Done as e ->
        Log.info (fun f ->
            f "send: fd has been closed for writing, stopping thread");
        send_exit send e
    | Unix.Unix_error (Unix.EBADF, _, _) as e ->
        Log.info (fun f ->
            f "send: EBADFD: connection has been closed, stopping thread");
        send_exit send e
    | Unix.Unix_error (Unix.ECONNREFUSED, _, _) as e ->
        Log.info (fun f -> f "send: ECONNREFUSED: stopping thread");
        send_exit send e

  let zero = Cstruct.create 0

  let recv_exit recv e =
    with_lock recv.m (fun () ->
        recv.exit_status <- Some e;
        Condition.signal recv.c;
        if Queue.is_empty recv.waiters then Queue.push zero recv.q
        else
          let waiter = Queue.pop recv.waiters in
          (* A caller is blocked in recv already *)
          Luv_lwt.in_lwt_async (fun () -> Lwt.wakeup_later waiter zero))

  let receive_thread fd recv min_read_buffer_size =
    try
      (* Many packets are small ACKs so cache an allocated buffer *)
      let allocation_size = Constants.mib in
      let recv_buffer = ref (Cstruct.create allocation_size) in
      while true do
        if Cstruct.length !recv_buffer < min_read_buffer_size then
          recv_buffer := Cstruct.create allocation_size;
        let n = SR.recv fd !recv_buffer in
        if n == 0 then raise Done;
        let buf = Cstruct.sub !recv_buffer 0 n in
        recv_buffer := Cstruct.shift !recv_buffer n;
        Log.debug (fun f -> f "recv %d" n);
        with_lock recv.m (fun () ->
            let handled = ref false in
            while not !handled do
              if Queue.is_empty recv.waiters then
                if
                  (* No-one is waiting so consider queueing the buffer *)
                  n + recv.len > max_buffer
                then Condition.wait recv.c recv.m
                  (* Note we need to check recv.waiters again *)
                else (
                  Queue.push buf recv.q;
                  recv.len <- recv.len + n;
                  handled := true)
              else
                let waiter = Queue.pop recv.waiters in
                (* A caller is blocked in recv already *)
                Luv_lwt.in_lwt_async (fun () -> Lwt.wakeup_later waiter buf);
                handled := true
            done
            (* Is someone already waiting *))
      done
    with
    | Done as e ->
        Log.info (fun f ->
            f "recv: fd has been closed for reading, stopping thread");
        recv_exit recv e
    | Unix.Unix_error (Unix.EBADF, _, _) as e ->
        Log.info (fun f ->
            f "recv: EBADFD: connection has been closed, stopping thread");
        recv_exit recv e
    | Unix.Unix_error (Unix.ECONNREFUSED, _, _) as e ->
        Log.info (fun f -> f "recv: ECONNREFUSED: stopping thread");
        recv_exit recv e
    | e ->
        Log.err (fun f ->
            f "recv: unexpected exception: %s: stopping thread"
              (Printexc.to_string e));
        recv_exit recv e

  let of_bound_fd ?(min_read_buffer_size = 65536) fd =
    let send = make_one_direction () in
    let send_t = Thread.create (fun () -> send_thread fd send) () in
    let recv = make_one_direction () in
    let recv_t =
      Thread.create (fun () -> receive_thread fd recv min_read_buffer_size) ()
    in
    {
      fd;
      closed = false;
      closed_m = Mutex.create ();
      send;
      send_t;
      recv;
      recv_t;
    }

  let send flow bufs =
    let len = Cstructs.len bufs in
    let rec loop () =
      let action =
        with_lock flow.send.m (fun () ->
            if flow.send.shutdown_request then `Exception End_of_file
            else
              match flow.send.exit_status with
              | Some e -> `Exception e
              | None ->
                  let available = max_buffer - flow.send.len in
                  let can_send = min len available in
                  let remaining =
                    if can_send > 0 then (
                      List.iter
                        (fun buf -> Queue.push buf flow.send.q)
                        (Cstructs.sub bufs 0 can_send);
                      flow.send.len <- flow.send.len + can_send;
                      Condition.signal flow.send.c;
                      Cstructs.shift bufs can_send)
                    else bufs
                  in
                  if Cstructs.len remaining > 0 then (
                    (* Too much data is queued. We will wait and this will add backpressure *)
                    let t, u = Lwt.wait () in
                    Queue.push u flow.send.waiters;
                    `WaitingForSpace t)
                  else `Done)
      in
      match action with
      | `Exception e -> Lwt.fail e
      | `Done -> Lwt.return_unit
      | `WaitingForSpace t ->
          let open Lwt.Infix in
          t >>= fun () -> loop ()
    in
    loop ()

  let recv flow =
    let action =
      with_lock flow.recv.m (fun () ->
          if not (Queue.is_empty flow.recv.q) then (
            (* A buffer is already queued *)
            let buf = Queue.pop flow.recv.q in
            flow.recv.len <- flow.recv.len - Cstruct.length buf;
            Condition.signal flow.recv.c;
            `Done buf)
          else
            let t, u = Lwt.wait () in
            Queue.push u flow.recv.waiters;
            Condition.signal flow.recv.c;
            `WaitForData t)
    in
    match action with `Done buf -> Lwt.return buf | `WaitForData t -> t

  let shutdown_write flow =
    with_lock flow.send.m (fun () ->
        if not flow.send.shutdown_request then (
          flow.send.shutdown_request <- true;
          Condition.signal flow.send.c));
    try Unix.shutdown flow.fd Unix.SHUTDOWN_SEND
    with Unix.Unix_error (Unix.ENOTCONN, _, _) -> ()

  let shutdown_read flow =
    try Unix.shutdown flow.fd Unix.SHUTDOWN_RECEIVE
    with Unix.Unix_error (Unix.ENOTCONN, _, _) -> ()

  let close flow =
    with_lock flow.closed_m (fun () ->
        if flow.closed then ()
        else (
          flow.closed <- true;
          shutdown_write flow;
          shutdown_read flow;
          with_lock flow.send.m (fun () ->
              while flow.send.exit_status = None do
                Condition.wait flow.send.c flow.send.m
              done);
          with_lock flow.recv.m (fun () ->
              while flow.recv.exit_status = None do
                Condition.wait flow.recv.c flow.recv.m
              done);
          Unix.close flow.fd;
          Thread.join flow.recv_t;
          Thread.join flow.send_t))
end

module Test_IO = Make (struct
  let send = Writev.writev
  let recv = Read.read
end)

open Test_IO

let%test_unit "socketpair" =
  if Sys.os_type <> "Win32" then
    let a, b = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    Lwt_main.run
      (let open Lwt.Infix in
      let a_flow = of_bound_fd a in
      let b_flow = of_bound_fd b in
      let rec loop () =
        Lwt.catch
          (fun () ->
            send a_flow [ Cstruct.of_string "hello" ] >>= fun () ->
            Lwt.return true)
          (function
            | Unix.Unix_error (Unix.ENOTCONN, _, _) -> Lwt.return false
            | e -> Lwt.fail e)
        >>= function
        | false -> Lwt.return_unit
        | true -> Lwt_unix.sleep 1. >>= fun () -> loop ()
      in
      let _ = loop () in
      recv b_flow >>= fun buf ->
      let n = Cstruct.length buf in
      if n <> 5 then failwith (Printf.sprintf "recv returned %d, expected 5" n);
      let received = Cstruct.(to_string (sub buf 0 n)) in
      if received <> "hello" then
        failwith
          (Printf.sprintf "recv returned '%s', expected 'hello'" received);
      close a_flow;
      close b_flow;
      Lwt.return_unit)
