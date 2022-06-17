(* A debug tool, intended to check the I/O subsystem is working correctly. *)

open Lwt.Infix

let curl _verbose urls =
  let module C = Mirage_channel.Make(Host.Sockets.Stream.Tcp) in
  let fetch host port path =
    let path = if path = "" then "/" else path in
    Host.Dns.getaddrinfo host Unix.PF_INET
    >>= function
    | [] ->
      Printf.printf "unable to lookup %s\n" host;
      Lwt.return_unit
    | Ipaddr.V6 _ :: _ ->
      Printf.printf "IPv6 not currently supported.\n";
      Lwt.return_unit
    | Ipaddr.V4 ipv4 :: _ ->
      Printf.printf "connecting to %s:%d\n" (Ipaddr.V4.to_string ipv4) port;
      Host.Sockets.Stream.Tcp.connect (Ipaddr.V4 ipv4, port)
      >>= function
      | Error (`Msg m) ->
        Printf.printf "unable to connect: %s\n" m;
        Lwt.return_unit
      | Ok socket ->
        Printf.printf "connected\n";
        Lwt.finalize
          (fun () ->
            let c = C.create socket in
            let request = "GET " ^ path ^ " HTTP/1.0\r\nHost: " ^ host ^ "\r\nConnection: close\r\n\r\n" in
            Printf.printf "writing\n%s\n" request;
            C.write_string c request 0 (String.length request);
            C.flush c
            >>= function
            | Error e ->
              Printf.printf "error sending request: %s\n" (Fmt.str "%a" C.pp_write_error e);
              Lwt.return_unit
            | Ok () ->
              let rec loop () =
                C.read_some c >>= function
                | Ok `Eof        -> Lwt.return_unit
                | Error e        ->
                  Printf.printf "error reading response: %s\n" (Fmt.str "%a" C.pp_error e);
                  Lwt.return_unit
                | Ok (`Data buf) ->
                  print_string (Cstruct.to_string buf);
                  loop () in
              loop ()
          ) (fun () ->
            Host.Sockets.Stream.Tcp.close socket
           ) in
  try
    Host.Main.run begin
      Lwt_list.iter_s (fun url ->
        let uri = Uri.of_string url in
        if Uri.scheme uri <> Some "http" then begin
          Printf.printf "only http:// URLs are currently supported by this debug tool\n";
          Lwt.return_unit
        end else begin
          Printf.printf "trying URL %s\n" url;
          let path = Uri.path uri in
          match Uri.host uri, Uri.port uri with
          | Some host, Some port ->
            fetch host port path
          | Some host, None ->
            fetch host 80 path
          | _, _ ->
            Printf.printf "unable to parse host and port from URL\n";
            Lwt.return_unit
          end
      ) urls
    end
  with e ->
    Printf.printf "Host.Main.run caught exception %s: %s\n" (Printexc.to_string e) (Printexc.get_backtrace ())