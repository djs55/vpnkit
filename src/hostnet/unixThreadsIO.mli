module type SendRecv = sig
  val send : Unix.file_descr -> Cstructs.t -> int
  (** OS call to send or write *)

  val recv : Unix.file_descr -> Cstruct.t -> int
  (** OS call to read or recv *)
end

module Make (SR : SendRecv) : sig
  type flow
  (** a flow is an open connection with an RX queue and thread and TX queue and thread. *)

  val of_bound_fd : ?min_read_buffer_size:int -> Unix.file_descr -> flow
  (** create a flow from a file descriptor. If the flow is datagram based then we can use
      min_read_buffer_size to ensure the calls to read always provide enough space for a
      whole datagram *)

  val send : flow -> Cstructs.t -> unit Lwt.t
  (** add buffers to the TX queue *)

  val recv : flow -> Cstruct.t Lwt.t
  (** receive a buffer *)

  val close : flow -> unit
  (** close the flow, including the underlying file descriptor *)

  val shutdown_write : flow -> unit
  (** close the write side of the flow *)

  val shutdown_read : flow -> unit
  (** close the read side of the flow *)
end
