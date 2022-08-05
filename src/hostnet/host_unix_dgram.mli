(** A simple thread-per-socket AF_UNIX SOCK_DRAM send/recv implementation to work around
    the lack of support in libuv.
    
    This will be used for a single ethernet socket at a time, so scalability isn't required.
    *)

include Sig.UNIX_DGRAM

include Sig.CONN with type flow := flow

type server
(** A Unix domain socket which can receive datagram sockets *)

type address = string
(** Path of a listening Unix domain socket *)

val bind: ?description:string -> address -> server Lwt.t
(** Bind a server to an address *)

val listen: server -> (flow -> unit Lwt.t) -> unit
(** Accept connections forever, calling the callback with each one.
    Connections are closed automatically when the callback finishes. *)

val shutdown: server -> unit Lwt.t
(** Stop accepting connections on the given server *)