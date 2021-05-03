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

val in_luv: (('a -> unit) -> unit) -> 'a Lwt.t
(** [in_luv f] is called from Lwt to run [f return] in the default Luv event loop.
    The function [return] may be used to return values to the Lwt caller. *)

val in_luv_async: (unit -> unit) -> unit
(** [in_luv_async f] is called from Lwt to run [f ()] in the default Luv event loop. *)

val in_lwt_async: (unit -> unit) -> unit
(** [run_in_lwt f] is called from Luv to run [f ()] in the default Lwt event loop. *)


val run: 'a Lwt.t -> 'a
(** [run t] evaluates [t] with the default Luv event loop. *)
