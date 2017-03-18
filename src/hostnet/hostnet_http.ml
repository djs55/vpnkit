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

  module Ch = Channel.Make(Tcp)
  module IO = Cohttp_mirage_io.Make(Ch)
  module Request = Cohttp.Request.Make(IO)
  module Response = Cohttp.Response.Make(IO)

  type t = unit

  let destroy t =
    Lwt.return_unit

  let create () =
    Lwt.return_unit

  let handle_tcp ~t =
    let listeners port =
      Log.debug (fun f -> f "TCP handshake complete");
      Some (fun flow ->
        let c = Ch.create flow in
        Request.read c
        >>= function
        | `Eof ->
          Log.info (fun f -> f "EOF on HTTP");
          Tcp.close flow
        | `Invalid x ->
          Log.info (fun f -> f "Invalid HTTP request: %s" x);
          Tcp.close flow
        | `Ok request ->
          Log.info (fun f -> f "HTTP %s" (Uri.to_string @@ Cohttp.Request.uri request));
          Tcp.close flow
      ) in
    Lwt.return listeners

end
