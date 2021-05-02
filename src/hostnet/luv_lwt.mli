(* Both Lwt and Luv have their own schedulers which we run on separate Threads.
   To use one API from another we enqueue it and send a notification to process jobs in the queue.

   A typical example would be:

```
let do_some_io () : unit Lwt.t =
    let t, u = Lwt.task () in
    Luv_lwt.run_in_luv (fun () ->
        (* Now we're in the Luv event loop *)
        Luv.Do.something _ @@ Luv_lwt.run_in_lwt @@ Lwt.wakeup_later u
    );
    t
```
*)

module type Remote_work_queue = sig
    type 'a t
    (** A queue of jobs to be executed inside inside a remote event loop *)

    val make: ('a -> unit) -> 'a t
    (** [make run] creates a queue whose jobs will be executed by [run]. *)

    val push: 'a t -> 'a -> unit
    (** [push q x] pushes a job [x] to the queue [q]. This can be run from any thread. *)
end

module Run_in_lwt: Remote_work_queue

val run_in_luv: (unit -> unit) -> unit
(** [run_in_luv f] runs [f ()] in the default Luv event loop.
    This assumes that Luv is not thread-safe and therefore we must interact with it from one thread. *)

val run_in_lwt: (unit -> unit) -> unit -> unit
(** [run_in_lwt f] runs [f ()] in the default Lwt event loop.
    This assumes that Lwt is not thread-safe and therefore we must interact with it from one thread. *)

val in_luv: (('a -> unit) -> unit) -> 'a Lwt.t

val run: 'a Lwt.t -> 'a
(** [run t] evaluates [t] with the default Luv event loop. *)

type 'a u
(** A task result which can be sent from a foreign thread *)

val task: unit -> 'a Lwt.t * 'a u

val wakeup_later: 'a u -> 'a -> unit

exception Error of Luv.Error.t