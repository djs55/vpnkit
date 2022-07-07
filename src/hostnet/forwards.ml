let src =
    let src = Logs.Src.create "gateway_forwards" ~doc:"Manages IP forwarding from the gateway IP" in
    Logs.Src.set_level src (Some Logs.Info);
    src

module Log = (val Logs.src_log src : Logs.LOG)

module Protocol = struct
    type t = [
    | `Tcp
    ]
    (* consider UDP later *)

    open Ezjsonm
    let to_json t = string (match t with `Tcp -> "tcp")
    let of_json j = match get_string j with
    | "tcp" -> `Tcp
    | _ -> raise (Parse_error(j, "protocol should be tcp"))
end

type forward = {
    protocol: Protocol.t;
    dst_prefix: Ipaddr.V4.Prefix.t;
    dst_port: int;
    path: string; (* unix domain socket path *)
  }

let forward_to_json t =
    let open Ezjsonm in
    dict [
        "protocol", Protocol.to_json t.protocol;
        "dst_prefix", string (Ipaddr.V4.Prefix.to_string t.dst_prefix);
        "dst_port", int t.dst_port;
        "path", string t.path;
    ]

let forward_of_json j =
    let open Ezjsonm in
    let protocol = Protocol.of_json @@ find j [ "protocol" ] in
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

module Read_some(FLOW: Mirage_flow.S) = (struct
    type flow = {
        mutable remaining: Cstruct.t;
        flow: FLOW.flow;
    }
    let create flow = {
        remaining = Cstruct.create 0;
        flow = flow;
    }
    type error = FLOW.error
    let pp_error = FLOW.pp_error
    type write_error = FLOW.write_error
    let pp_write_error = FLOW.pp_write_error
    let read_some flow len =
        let open Lwt.Infix in
        let rec loop acc len =
            if Cstruct.length flow.remaining = 0 then begin
                FLOW.read flow.flow
                >>= function
                | Error e -> Lwt.return (Error e)
                | Ok (`Data buf) ->
                    flow.remaining <- buf;
                    loop acc len
                | Ok `Eof -> Lwt.return (Ok `Eof)
            end else begin
                if Cstruct.length flow.remaining <= len then begin
                    let take = flow.remaining in
                    flow.remaining <- Cstruct.create 0;
                    loop (take :: acc) (len - (Cstruct.length take))
                end else begin
                    let take, leave = Cstruct.split flow.remaining len in
                    flow.remaining <- leave;
                    Lwt.return @@ Ok (`Data (List.rev (take :: acc)))
                end
            end in
        loop [] len
    let read flow =
        if Cstruct.length flow.remaining > 0 then begin
            let result = flow.remaining in
            flow.remaining <- Cstruct.create 0;
            Lwt.return @@ Ok (`Data result)
        end else FLOW.read flow.flow
    let write flow = FLOW.write flow.flow
    let writev flow = FLOW.writev flow.flow
    let close flow = FLOW.close flow.flow
end: sig
    include Mirage_flow.S
    val create: FLOW.flow -> flow
    val read_some: flow -> int -> (Cstructs.t Mirage_flow.or_eof, error) result Lwt.t
end)

module Handshake(FLOW: Mirage_flow.S) = struct
    module Message = struct
      type t = Cstruct.t

    end
    module Request = struct
        type t = {
            protocol: Protocol.t;
            src_ip: Ipaddr.V4.t;
            src_port: int;
            dst_ip: Ipaddr.V4.t;
            dst_port: int;
        }
        open Ezjsonm
        let of_json j =
            let protocol = Protocol.of_json @@ find j [ "protocol" ] in
            let src_ip = match Ipaddr.V4.of_string @@ get_string @@ find j [ "src_ip" ] with
            | Error (`Msg m) -> raise (Parse_error(j, "src_ip should be an IPv4 address: " ^ m))
            | Ok x -> x in
            let src_port = get_int @@ find j [ "src_port" ] in
            let dst_ip = match Ipaddr.V4.of_string @@ get_string @@ find j [ "dst_ip" ] with
            | Error (`Msg m) -> raise (Parse_error(j, "dst_ip should be an IPv4 address: " ^ m))
            | Ok x -> x in
            let dst_port = get_int @@ find j [ "dst_port" ] in
            {
                protocol; src_ip; src_port; dst_ip; dst_port;
            }
    end
    module Response = struct
        type t = {
            accepted: bool
        }
        open Ezjsonm
        let of_json j =
            let accepted = get_bool @@ find j [ "accepted" ] in
            {
                accepted;
            }
    end

end

module Tcp = struct
  let mem (dst_ip, dst_port) = List.exists (fun f -> f.protocol = `Tcp && Ipaddr.V4.Prefix.mem dst_ip f.dst_prefix && f.dst_port = dst_port) !all
  let find (dst_ip, dst_port) =
    let f = List.find (fun f -> f.protocol = `Tcp && Ipaddr.V4.Prefix.mem dst_ip f.dst_prefix && f.dst_port = dst_port) !all in
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