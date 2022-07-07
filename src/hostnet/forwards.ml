let src =
    let src = Logs.Src.create "gateway_forwards" ~doc:"Manages IP forwarding from the gateway IP" in
    Logs.Src.set_level src (Some Logs.Info);
    src

module Log = (val Logs.src_log src : Logs.LOG)

type protocol =
  | Tcp
  (* consider UDP later *)

type forward = {
    protocol: protocol;
    dst_prefix: Ipaddr.V4.Prefix.t;
    dst_port: int;
    path: string; (* unix domain socket path *)
  }

let forward_to_json t =
    let open Ezjsonm in
    dict [
        "protocol", string (match t.protocol with Tcp -> "tcp");
        "dst_prefix", string (Ipaddr.V4.Prefix.to_string t.dst_prefix);
        "dst_port", int t.dst_port;
        "path", string t.path;
    ]

let forward_of_json j =
    let open Ezjsonm in
    let protocol = match get_string @@ find j [ "protocol" ] with
      | "tcp" -> Tcp
      | _ -> raise (Parse_error(j, "protocol should be tcp")) in
    let dst_port = get_int @@ find j [ "dst_port" ] in
    let path = get_string @@ find j [ "path" ] in
    let dst_prefix = match Ipaddr.V4.Prefix.of_string @@ get_string @@ find j [ "dst_prefix" ] with
      | Error (`Msg m) -> raise (Parse_error(j, "dst_ip should be an IPv4 prefix: " ^ m))
      | Ok x -> x in
    {
        protocol; dst_prefix; dst_port; path;
    }

type t = forward list

let to_json = Ezjsonm.list forward_to_json
let of_json = Ezjsonm.get_list forward_of_json

let to_string x = Ezjsonm.to_string @@ to_json x
let of_string x =
    try
        Ok (of_json @@ Ezjsonm.from_string x)
    with Ezjsonm.Parse_error(_v, msg) ->
        Error (`Msg msg)

let dynamic = ref []

let static = ref []

let all = ref []

let set_static xs =
    static := xs;
    all := !static @ !dynamic;
    Log.info (fun f -> f "New Gateway forward configuration: %s" (to_string !all))

let update xs =
    dynamic := xs;
    all := !static @ !dynamic;
    Log.info (fun f -> f "New Gateway forward configuration: %s" (to_string !all))

module Tcp = struct
  let mem (dst_ip, dst_port) = List.exists (fun f -> f.protocol = Tcp && Ipaddr.V4.Prefix.mem dst_ip f.dst_prefix && f.dst_port = dst_port) !all
  let find (dst_ip, dst_port) =
    let f = List.find (fun f -> f.protocol = Tcp && Ipaddr.V4.Prefix.mem dst_ip f.dst_prefix && f.dst_port = dst_port) !all in
    f.path
end

module Make
    (Clock: Mirage_clock.MCLOCK)
    (Ip: Tcpip.Ip.S with type ipaddr = Ipaddr.V4.t)
    (Tcp_flow:Mirage_flow_combinators.SHUTDOWNABLE)
    (Socket: Sig.SOCKETS)
= struct
    open Lwt.Infix

    module Proxy =
      Mirage_flow_combinators.Proxy(Clock)(Tcp_flow)(Socket.Stream.Unix)

    let with_forwarded_connection remote =
        let listeners _port =
          Log.debug (fun f -> f "TCP handshake complete");
          let process flow =
            Lwt.catch
              (fun () ->
                Lwt.finalize (fun () ->
                    Proxy.proxy flow remote
                    >>= function
                    | Error e ->
                      Log.debug (fun f ->
                          f "TCP proxy failed with %a" Proxy.pp_error e);
                      Lwt.return_unit
                    | Ok (_l_stats, _r_stats) ->
                      Lwt.return_unit
                  ) (fun () -> Tcp_flow.close flow)
              ) (fun e ->
                Log.warn (fun f -> f "tcp_forward caught exception: %s" (Printexc.to_string e));
                Lwt.return_unit
              )
          in Some process
        in
        Lwt.return listeners

    let handler ~dst:(dst_ip, dst_port) =
        if not(Tcp.mem (dst_ip, dst_port))
        then Lwt.return None
        else begin
            let path = Tcp.find (dst_ip, dst_port) in
            Socket.Stream.Unix.connect path
            >>= function
            | Error (`Msg m) ->
                Log.info (fun f -> f "TCP forward %a, %d -> %s: %s, returning RST" Ipaddr.V4.pp dst_ip dst_port path m);
                Lwt.return None
            | Ok remote ->
                Lwt.return @@ Some (with_forwarded_connection remote)
        end

end