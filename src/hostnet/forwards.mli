module Protocol: sig
  type t = [ `Tcp ]
  (* consider UDP later *)
end

type forward = {
  protocol : Protocol.t;
  dst_prefix : Ipaddr.V4.Prefix.t;
  dst_port : int;
  path : string; (* unix domain socket path *)
}

type t = forward list

val to_string : t -> string
val of_string : string -> (t, [ `Msg of string ]) result

val set_static : t -> unit
(** update the static forwarding table *)

val update : t -> unit
(** update the dynamic forwarding table *)

module Tcp : sig
  val mem : Ipaddr.V4.t * int -> bool
  (** [mem dst_ip dst_port] is true if there is a rule to forward TCP to [dst_ip,dst_port]. *)

  val find : Ipaddr.V4.t * int -> string
  (** [find dst_ip dst_port] returns the internal path to forward the TCP connection to. *)
end

module Make
    (Clock: Mirage_clock.MCLOCK)
    (Ip : Tcpip.Ip.S with type ipaddr = Ipaddr.V4.t)
    (Tcp : Mirage_flow_combinators.SHUTDOWNABLE)
    (Socket : Sig.SOCKETS) : sig
  val handler :
    src:Ipaddr.V4.t * int ->
    dst:Ipaddr.V4.t * int ->
    (int -> (Tcp.flow -> unit Lwt.t) option) Lwt.t option Lwt.t
  (** Intercept outgoing TCP flows and redirect to a proxy *)
end
