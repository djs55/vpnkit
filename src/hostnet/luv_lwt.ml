
type queue = {
  (* Note we have to wake the Lwt threads with a unit. *)
  pending: unit Lwt.u Queue.t;
  mutable notification: int;
  m: Mutex.t;
}

let flush queue () =
  (* Called on the Lwt thread to wakeup a batch of blocked threads. *)
  Mutex.lock queue.m;
  Queue.iter (fun u -> Lwt.wakeup_later u ()) queue.pending;
  Queue.clear queue.pending;
  Mutex.unlock queue.m

let unit_wakeup_later queue u =
  (* Called on an arbitrary libuv callback thread to wakeup an Lwt task. *)
  Mutex.lock queue.m;
  if Queue.is_empty queue.pending then begin
      Lwt_unix.send_notification queue.notification;
  end;
  Queue.push u queue.pending;
  Mutex.unlock queue.m

let make_wakeup_queue () =
  let queue = {
    pending = Queue.create();
    notification = 0; (* initialised below *)
    m = Mutex.create();
  } in
  queue.notification <- Lwt_unix.make_notification (flush queue);
  queue

let unit_wakeup_later = unit_wakeup_later (make_wakeup_queue ())

(* Wrap the result value here. *)
type 'a task = {
  result: 'a option ref;
  u: unit Lwt.u;
}

let task () =
  let result = ref None in
  let t, u = Lwt.task () in
  let u = { result; u; } in
  Lwt.bind t (fun () ->
    match !result with
    | None -> Lwt.fail_with "task wakeup with no value"
    | Some x -> Lwt.return x
  ), u

let wakeup_later u x =
  u.result := Some x;
  unit_wakeup_later u.u
