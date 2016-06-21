open Lwt.Infix
open Uwt_compat

let src =
  let src = Logs.Src.create "dns" ~doc:"Resolve DNS queries on the host" in
  Logs.Src.set_level src (Some Logs.Debug);
  src

module Log = (val Logs.src_log src : Logs.LOG)

let parse_dns buf =
  let len = Cstruct.len buf in
  let buf = Dns.Buf.of_cstruct buf in
  (len, Dns.Protocol.Server.parse (Dns.Buf.sub buf 0 len))

let string_of_dns dns =
  match dns with
  | (len, None) ->
    Printf.sprintf "Unparsable DNS packet length %d" len
  | (_, Some request) ->
    Dns.Packet.to_string request

let tidstr_of_dns dns =
  match dns with
  | (_, None) -> "----"
  | (_, Some { Dns.Packet.id; _ }) -> Printf.sprintf "%04x" id

module Make(Ip: V1_LWT.IPV4) (Udp:V1_LWT.UDPV4) (Resolv_conf: Sig.RESOLV_CONF) = struct

type transaction = {
  mutable resolvers : (Ipaddr.t * int) list;
  mutable used_resolvers : (Ipaddr.t * int) list;
  mutable last_use: float;
}

let table = Hashtbl.create 7

let start_reaper () =
  let rec loop () =
    Lwt_unix.sleep 60.
    >>= fun () ->
    let snapshot = Hashtbl.copy table in
    let now = Unix.gettimeofday () in
    Hashtbl.iter (fun k trec ->
      if now -. trec.last_use > 60. then begin
        Log.debug (fun f -> f "DNS[%04x] Expiring DNS trec" k);
        Hashtbl.remove table k
      end
    ) snapshot;
    loop () in
  loop ()

let input ~ip ~udp ~src ~dst ~src_port buf =
  if List.mem dst (Ip.get_ip ip) then begin

  let src_str = Ipaddr.V4.to_string src in
  let dst_str = Ipaddr.V4.to_string dst in

  let dns = parse_dns buf in

  let remove_tid dns =
    match dns with
    | (_, None) -> ()
    | (_, Some { Dns.Packet.id; _ }) -> Hashtbl.remove table id in

  Log.debug (fun f -> f "DNS[%s] %s:%d -> %s %s" (tidstr_of_dns dns) src_str src_port dst_str (string_of_dns dns));

  match dns with
  | (_, None) -> begin
    Resolv_conf.get ()
    >>= function
    | r::_ -> Lwt.return_some r
    | _ -> Lwt.return_none
  end
  | (_, Some { Dns.Packet.id = tid; _ }) -> begin
    Lwt.catch
      (fun () ->
        let trec = Hashtbl.find table tid in
        let r, rs, urs = match trec.resolvers with
          | r::rs -> (r,rs,r::trec.used_resolvers)
          | _ -> match List.rev trec.used_resolvers with
            | r::rs -> (r,rs,[])
            | _  -> assert false (* resolvers and used_resolvers cannot both be empty *)
        in
        Log.debug (fun f -> f "DNS[%s] Retry" (tidstr_of_dns dns));
        trec.resolvers <- rs;
        trec.used_resolvers <- urs;
        trec.last_use <- Unix.gettimeofday ();
        Lwt.return_some r
      )
      (function
      | Not_found -> begin
         (* Re-read /etc/resolv.conf on every request. This ensures that
            changes to DNS on sleep/resume or switching networks are reflected
            immediately. The file is very small, and parsing it shouldn't be
            too slow. *)
        Resolv_conf.get ()
        >>= function
        | r::rs -> begin
          let last_use = Unix.gettimeofday () in
          let trec = { resolvers = rs; used_resolvers = r :: []; last_use } in
          Hashtbl.replace table tid trec;
          Lwt.return_some r
        end
        | _ -> Lwt.return_none
      end
      | ex -> Lwt.fail ex
      )
  end;
  >>= function
  | Some (dst, dst_port) -> begin
    let fd = Uwt.Udp.init () in
    (* FIXME(djs55): why of_unix_sockaddr_exn? *)
    let remote_sockaddr =
      let unix = Unix.ADDR_INET(Unix.inet_addr_of_string @@ Ipaddr.to_string dst, dst_port) in
      Uwt_base.Conv.of_unix_sockaddr_exn unix in
    Log.debug (fun f -> f "DNS[%s] Forwarding to %s" (tidstr_of_dns dns) (Ipaddr.to_string dst));
    Lwt.catch
      (fun () ->
        Uwt.Udp.send_ba ~pos:buf.Cstruct.off ~len:buf.Cstruct.len ~buf:buf.Cstruct.buffer fd remote_sockaddr
      ) (fun e ->
        Log.err (fun f -> f "DNS[%s] send failed with %s" (tidstr_of_dns dns) (Printexc.to_string e));
        Lwt.return ()
      )
    >>= fun () ->
    let receiver =
      let buf = Cstruct.create 4096 in
      Lwt.catch
        (fun () ->
          Uwt.Udp.recv_ba ~pos:buf.Cstruct.off ~len:buf.Cstruct.len ~buf:buf.Cstruct.buffer fd
          >>= fun recv ->
          if recv.Uwt.Udp.is_partial then begin
            Log.err (fun f -> f "DNS[%s] dropping partial response" (tidstr_of_dns dns));
            Lwt.return `Error
          end else if recv.Uwt.Udp.sockaddr = None then begin
            Log.err (fun f -> f "DNS[%s] dropping response from unknown sockaddr" (tidstr_of_dns dns));
            Lwt.return `Error
          end else begin
            Lwt.return (`Result (Cstruct.sub buf 0 recv.Uwt.Udp.recv_len))
          end
        ) (fun e ->
           Log.err (fun f -> f "DNS[%s] recv failed with %s" (tidstr_of_dns dns) (Printexc.to_string e));
           Lwt.return `Error
        ) in
    let timeout = Uwt.Timer.sleep 5000 >>= fun () -> Lwt.return `Timeout in
    Lwt.pick [ receiver; timeout ]
    >>= fun r ->
    let _ = Uwt.Udp.close fd in
    (* FIXME(djs55): some result here *)
    match r with
    | `Error ->
      Lwt.return_unit
    | `Timeout ->
      Log.err (fun f -> f "DNS[%s] timed out after 5s" (tidstr_of_dns dns));
      Lwt.return_unit
    | `Result buffer ->
      Log.debug (fun f -> f "DNS[%s] %s:%d <- %s %s" (tidstr_of_dns dns) src_str src_port dst_str (string_of_dns (parse_dns buffer)));
      remove_tid dns;
      Udp.write ~source_port:53 ~dest_ip:src ~dest_port:src_port udp buffer
    end
  | None ->
    Log.err (fun f -> f "DNS[%s] No upstream DNS server configured: dropping request" (tidstr_of_dns dns));
    Lwt.return_unit
  end else Lwt.return_unit
end
