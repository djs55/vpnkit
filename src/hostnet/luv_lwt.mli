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
(** [run_in_luv f] runs [f ()] in the default Luv event loop. *)

val run: 'a Lwt.t -> 'a
(** [run t] evaluates [t] with the default Luv event loop. *)

type 'a u
(** A task result which can be sent from a foreign thread *)

val task: unit -> 'a Lwt.t * 'a u

val wakeup_later: 'a u -> 'a -> unit

exception Error of Luv.Error.t