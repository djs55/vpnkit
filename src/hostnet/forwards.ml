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