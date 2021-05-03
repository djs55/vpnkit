
module Result = struct
  include Result
  let get_ok = function
    | Error _ -> invalid_arg "result is Error _"
    | Ok x -> x
end

module type Notification = sig
  (** Wake up and run code in a remote event loop *)
  type t
  val create: (unit -> unit) -> t
  val send: t -> unit
end

module Lwt_notification : Notification = struct
  (** Run code in an Lwt event loop *)
  type t = int
  let create cb = Lwt_unix.make_notification cb
  let send = Lwt_unix.send_notification
end

module Luv_notification : Notification = struct
  (** Run code in the default Luv event loop *)
  type t = [ `Async ] Luv.Handle.t
  let create cb = Luv.Async.init (fun _t -> cb ()) |> Result.get_ok
  let send t = Luv.Async.send t |> Result.get_ok
end

module type Remote_work_queue = sig
  type 'a t
  val make: ('a -> unit) -> 'a t  
  val push: 'a t -> 'a -> unit
end

module Work_queue(N: Notification) : Remote_work_queue = struct
  (** A thread-safe queue of pending jobs which will be run in a remote event loop *)
  type 'a t = {
    pending: 'a Queue.t;
    run: 'a -> unit;
    mutable n: N.t option;
    (* Note: if this is only used from a single-threaded Lwt or Luv context, then
       the mutex is unnecessary. The tests below use pthreads so require the mutex. *)
    m: Mutex.t;
  }

  let flush t () =
    (* Called in the remote event loop to run pending jobs .*)
    Mutex.lock t.m;
    Queue.iter t.run t.pending;
    Queue.clear t.pending;
    Mutex.unlock t.m

  let push t x =
    (* Called on an arbitrary thread to queue a remote job. *)
    Mutex.lock t.m;
    (* We only need to send a notification if the queue is currently empty, otherwise
       one has already been sent. *)
    if Queue.is_empty t.pending then N.send (Option.get t.n);
    Queue.push x t.pending;
    Mutex.unlock t.m

  let make run =
    let t = {
      pending = Queue.create ();
      run = run;
      n = None; (* initialized below *)
      m = Mutex.create();
    } in
    t.n <- Some (N.create (flush t));
    t
end

module Run_in_lwt = Work_queue(Lwt_notification)

module Run_in_luv = Work_queue(Luv_notification)

let to_luv_default_loop = Run_in_luv.make (fun f -> f ())

let to_lwt_default_loop = Run_in_lwt.make (fun f -> f ())

let in_lwt_async f = Run_in_lwt.push to_lwt_default_loop f

let in_luv_async = Run_in_luv.push to_luv_default_loop

let in_luv f =
  let t, u = Lwt.task () in
  let wakeup_later x = in_lwt_async (fun () -> Lwt.wakeup_later u x) in
  in_luv_async (fun () -> f wakeup_later);
  t

let run t =
  (* Hopefully it's ok to create the async handle in this thread, even though the
     main loop runs in another thread. *)
  let stop_default_loop = Luv.Async.init (fun h ->
    Luv.Loop.stop (Luv.Loop.default ());
    Luv.Handle.close h ignore
  ) |> Result.get_ok in
  let luv = Thread.create (fun () ->
    ignore (Luv.Loop.run () : bool);
  ) () in
  (* With the luv event loop running in the background, we can evaluate [t] *)
  let result = Lwt_main.run t in
  Luv.Async.send stop_default_loop |> Result.get_ok;
  Thread.join luv;
  result

let%test "wakeup one task from a luv callback" =
  let t, u = Lwt.task () in
  let luv = Thread.create (fun () ->
    in_lwt_async (Lwt.wakeup_later u)
    ) () in
  Thread.join luv;
  Lwt_main.run t;
  true

let%test "wakeup lots of tasks from a luv callback" =
  let n = 1000 in
  let tasks = Array.init n (fun _ -> Lwt.task ()) |> Array.to_list in
  List.iteri (fun i (_, u) ->
    let _: Thread.t = Thread.create (fun i ->
      (* Introduce jitter *)
      Thread.delay 0.5;
      in_lwt_async (fun () -> Lwt.wakeup_later u i)
      ) i in
    ()
  ) tasks;
  Lwt_main.run begin
    let open Lwt.Infix in
    let ts = List.map fst tasks in
    Lwt_list.fold_left_s (fun acc b_t -> b_t >>= fun b -> Lwt.return (acc + b)) 0 ts
  end = (n * (n - 1)) / 2
