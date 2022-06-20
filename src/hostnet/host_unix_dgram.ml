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
        let result = f r in
        Luv_lwt.in_lwt_async (fun () -> Lwt.wakeup_later r.done_u result)
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
  Lwt.return {fd; send; recv}

let send flow buf = push flow.send buf

let recv flow buf = push flow.recv buf

let close flow =
  close flow.send;
  close flow.recv
