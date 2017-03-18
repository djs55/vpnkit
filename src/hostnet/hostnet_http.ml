open Lwt.Infix

let src =
  let src = Logs.Src.create "http" ~doc:"HTTP proxy" in
  Logs.Src.set_level src (Some Logs.Info);
  src

module Log = (val Logs.src_log src : Logs.LOG)

module Make
    (Ip: V1_LWT.IPV4 with type prefix = Ipaddr.V4.t)
    (Tcp:V1_LWT.TCPV4)
    (Socket: Sig.SOCKETS)
    (Time: V1_LWT.TIME)
    (Clock: V1.CLOCK) = struct

  type t = unit

  let destroy t =
    Lwt.return_unit

  let create () =
    Lwt.return_unit

  let handle_tcp ~t =
    let listeners port =
      Log.debug (fun f -> f "TCP handshake complete");
      Some (fun flow ->
        let module C = Channel.Make(Tcp) in
        let c = C.create flow in
        C.read_line c
        >>= fun bufs ->
        Log.info (fun f -> f "Read %s" (String.concat "" (List.map Cstruct.to_string bufs)));
        List.iter (C.write_buffer c) bufs;
        C.flush c
        >>= fun () ->
        Tcp.close flow
      ) in
    Lwt.return listeners

end
