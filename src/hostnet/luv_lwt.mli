type 'a queue
(** A queue of jobs to be executed inside an Lwt context *)

val make_queue: ('a -> unit) -> 'a queue
(** [make_queue run] creates a queue whose jobs will be executed by [run]. *)

val push: 'a queue -> 'a -> unit
(** [push q x] pushes a job [x] to the queue [q]. *)


type 'a task
(** A task which may be woken up from a foreign thrad *)

val task: unit -> 'a Lwt.t * 'a task

val wakeup_later: 'a task -> 'a -> unit

exception Error of Luv.Error.t