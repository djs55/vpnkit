type protocol =
  | Tcp
  (* consider UDP later *)

type forward = {
    protocol: protocol;
    dst_ip: Ipaddr.V4.t; (* needs to be a CIDR *)
    dst_port: int;
    path: string; (* unix domain socket path *)
  }

type t = forward list

val to_string: t -> string
val of_string: string -> (t, [`Msg of string]) result

val set_static: t -> unit
(** update the static forwarding table *)

val update: t -> unit
(** update the dynamic forwarding table *)

module Tcp: sig
  val mem: Ipaddr.V4.t * int -> bool
  (** [mem dst_ip dst_port] is true if there is a rule to forward TCP to [dst_ip,dst_port]. *)

  val find: Ipaddr.V4.t * int -> string
  (** [find dst_ip dst_port] returns the internal path to forward the TCP connection to. *)
end