module Make
    (Ip: V1_LWT.IPV4 with type prefix = Ipaddr.V4.t)
    (Udp: V1_LWT.UDPV4)
    (Tcp: Mirage_flow_s.SHUTDOWNABLE)
    (Socket: Sig.SOCKETS)
    (Dns_resolver: Sig.DNS)
    : sig

    type t
    (** An HTTP proxy instance with a fixed configuration *)

    val none: t
    (** Use no upstream proxies: connect to servers directly *)

    val create: ?http:string -> ?https:string -> ?exclude:string -> unit -> t Error.t
    (** Create a transparent HTTP forwarding instance which forwards HTTP
        to the proxy [http], HTTPS to the proxy [https] or connects directly
        if the URL matches [exclude]. *)

    type flow_cb = (int -> (Tcp.flow -> unit Lwt.t) option) Lwt.t
    (** The Mirage TCP/IP stack internally handles incoming flows with this
        signature of callback *)

    val handle: dst:(Ipaddr.V4.t * int) -> t:t -> [ `Handle of flow_cb | `Redirect of (Ipaddr.t * int) ] option
    (** Given a destination address and a configuration, decides how to process
        the data.
        [`Handle f] means the flow should be processed by the function [f].
        [`Redirect address] means the flow should be forwarded to [address].
      *)
end
