type 'a task
(** A task which may be woken up from a foreign thrad *)

val task: unit -> 'a Lwt.t * 'a task

val wakeup_later: 'a task -> 'a -> unit
