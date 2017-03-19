
module Config: sig
  type t
  (** Describes the upstream proxy to use *)

  val none: t
  (** Use no upstream proxy *)

  val to_string: t -> string

  val of_string: string -> (t, [ `Msg of string ]) result

end

module Make
    (Ip: V1_LWT.IPV4 with type prefix = Ipaddr.V4.t)
    (Tcp:V1_LWT.TCPV4)
    (Socket: Sig.SOCKETS)
    (Time: V1_LWT.TIME)
    (Clock: V1.CLOCK) : sig

  type t
  (** An HTTP proxy instance with a fixed configuration *)

  val create: upstream:Config.t -> unit -> t Lwt.t
  (** Create an HTTP proxy instance *)

  val handle_tcp: t:t -> (int -> (Tcp.flow -> unit Lwt.t) option) Lwt.t

  val destroy: t -> unit Lwt.t
end
