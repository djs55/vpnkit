open Lwt.Infix
open Sexplib.Std

let src =
  let src = Logs.Src.create "http" ~doc:"HTTP proxy" in
  Logs.Src.set_level src (Some Logs.Info);
  src

module Log = (val Logs.src_log src : Logs.LOG)

module Config = struct
  type upstream = {
    http: string;
    https: string;
  } [@@deriving sexp]

  type t = upstream option [@@deriving sexp]

  let none = None

  let to_string t = Sexplib.Sexp.to_string_hum @@ sexp_of_t t

  let of_string x =
    try
      Ok (t_of_sexp @@ Sexplib.Sexp.of_string x)
    with _ ->
      Error (`Msg (Printf.sprintf "Unable to parse: '%s'" x))
end

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

  type t = {
    config: Config.t;
  }

  let destroy t =
    Lwt.return_unit

  let create ~upstream () =
    Lwt.return { config = upstream }

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
