
type 'a queue = {
  (* Pending jobs to be run in an Lwt-safe context. *)
  pending: 'a Queue.t;
  (* Run a job *)
  run: 'a -> unit;
  mutable notification: int;
  m: Mutex.t;
}

let flush queue () =
  (* Called on the Lwt thread to trigger a bunch of jobs *)
  Mutex.lock queue.m;
  Queue.iter queue.run queue.pending;
  Queue.clear queue.pending;
  Mutex.unlock queue.m

let push queue x =
  (* Called on an arbitrary libuv callback to queue a job for Lwt *)
  Mutex.lock queue.m;
  if Queue.is_empty queue.pending then begin
      Lwt_unix.send_notification queue.notification;
  end;
  Queue.push x queue.pending;
  Mutex.unlock queue.m

let make_queue run =
  let queue = {
    pending = Queue.create ();
    run = run;
    notification = 0; (* initialised below *)
    m = Mutex.create();
  } in
  queue.notification <- Lwt_unix.make_notification (flush queue);
  queue


let unit_wakeup_later = make_queue (fun x -> Lwt.wakeup_later x ())
 
(* Wrap the result value here. *)
type 'a u = {
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
  push unit_wakeup_later u.u

exception Error of Luv.Error.t

let%test "wakeup one task from a luv callback" =
  let t, u = task () in
  let luv = Thread.create (fun () ->
    wakeup_later u ()
    ) () in
  Thread.join luv;
  Lwt_main.run t;
  true

let%test "wakeup lots of tasks from a luv callback" =
  let n = 1000 in
  let tasks = Array.init n (fun _ -> task ()) |> Array.to_list in
  List.iteri (fun i (_, u) ->
    let _: Thread.t = Thread.create (fun i ->
      (* Introduce jitter *)
      Thread.delay 0.5;
      wakeup_later u i
      ) i in
    ()
  ) tasks;
  Lwt_main.run begin
    let open Lwt.Infix in
    let ts = List.map fst tasks in
    Lwt_list.fold_left_s (fun acc b_t -> b_t >>= fun b -> Lwt.return (acc + b)) 0 ts
  end = (n * (n - 1)) / 2
