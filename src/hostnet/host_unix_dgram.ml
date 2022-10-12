let src =
  let src = Logs.Src.create "Datagram" ~doc:"Host SOCK_DGRAM implementation" in
  Logs.Src.set_level src (Some Logs.Info);
  src

module Log = (val Logs.src_log src : Logs.LOG)

module Sock_Dgram = UnixThreadsIO.Make (struct
  let send fd bufs =
    (* Each buffer is a separate datagram, so send separately. If we had a better API we
       could send a batch (e.g. io_uring?) *)
    List.fold_left (fun total buf ->
      try
        let n = Utils.cstruct_send fd buf in
        let len = Cstruct.length buf in
        if n <> len then
          Log.warn (fun f ->
              f "Utils.cstruct_send packet length %d but sent only %d" len n);
        total + n
      with Unix.Unix_error (Unix.ENOBUFS, _, _) ->
        (* If we're out of buffer space we have to drop the packet *)
        Log.warn (fun f -> f "ENOBUFS: dropping packet");
        total
      ) 0 bufs
  let recv = Utils.cstruct_recv 
end)

type flow = Sock_Dgram.flow

type error = [ `Closed | `Msg of string ]

let pp_error ppf = function
  | `Closed -> Fmt.string ppf "Closed"
  | `Msg m -> Fmt.string ppf m

type write_error = error

let pp_write_error = pp_error

open Lwt.Infix

let of_bound_fd ?(mtu=65536) fd = Sock_Dgram.of_bound_fd ~min_read_buffer_size:mtu fd |> Lwt.return

let send t buf = Sock_Dgram.send t [ buf ]

let recv t = Sock_Dgram.recv t

let read t =
  Sock_Dgram.recv t >>= fun buf ->
  let n = Cstruct.length buf in
  if n = 0 then Lwt.return @@ Ok `Eof else Lwt.return @@ Ok (`Data buf)

let read_into _t _buf =
  Lwt.return (Error (`Msg "read_into not implemented for SOCK_DGRAM"))

let write t buf = Sock_Dgram.send t [ buf ] >>= fun () -> Lwt.return @@ Ok ()

let writev t bufs =
  (* Assemble as a single packet *)
  let buf = Cstruct.concat bufs in
  write t buf

let close t =
  Sock_Dgram.close t;
  Lwt.return_unit

(* A server listens on a Unix domain socket for connections and then receives SOCK_DGRAM
   file descriptors. In case someone connects and doesn't know the protocol we have a text
   error message describing what the socket is really for. *)
type server = { fd : Unix.file_descr }
type address = string

let magic = "VMNET"

let error_message =
  "This socket receives SOCK_DGRAM file descriptors for sending and receiving \
   ethernet frames.\n\
   It cannot be used directly.\n"

let success_message = "OK"

(* For low-frequency tasks like binding a listening socket, we fork a pthread for one request. *)
let run_in_pthread f =
  let t, u = Lwt.task () in
  let (_ : Thread.t) =
    Thread.create
      (fun () ->
        try
          let result = f () in
          Luv_lwt.in_lwt_async (fun () -> Lwt.wakeup_later u result)
        with e -> Luv_lwt.in_lwt_async (fun () -> Lwt.wakeup_exn u e))
      ()
  in
  t

let finally f g =
  try
    let result = f () in
    g ();
    result
  with e ->
    g ();
    raise e

let connect address =
  let open Lwt.Infix in
  run_in_pthread (fun () ->
      try
        let s = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
        finally
          (fun () ->
            Unix.connect s (Unix.ADDR_UNIX address);
            let a, b = Unix.socketpair Unix.PF_UNIX Unix.SOCK_DGRAM 0 in
            (* We will send a and keep b. *)
            finally
              (fun () ->
                try
                  let (_ : int) =
                    Fd_send_recv.send_fd s (Bytes.of_string magic) 0
                      (String.length magic) [] a
                  in
                  let buf = Bytes.create (String.length error_message) in
                  let n = Unix.read s buf 0 (Bytes.length buf) in
                  let response = Bytes.sub buf 0 n |> Bytes.to_string in
                  if response <> success_message then
                    failwith ("Host_unix_dgram.connect: " ^ response);
                  Ok b
                with e ->
                  Unix.close b;
                  raise e)
              (fun () -> Unix.close a))
          (fun () -> Unix.close s)
      with e -> Error e)
  >>= function
  | Ok fd -> Sock_Dgram.of_bound_fd fd |> Lwt.return
  | Error e -> Lwt.fail e

let bind ?description:_ address =
  let open Lwt.Infix in
  run_in_pthread (fun () ->
      try
        let s = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
        try
          Unix.bind s (Unix.ADDR_UNIX address);
          Unix.listen s 5;
          Ok s
        with e ->
          Unix.close s;
          Error e
      with e -> Error e)
  >>= function
  | Ok fd -> Lwt.return { fd }
  | Error e -> Lwt.fail e

let listen server cb =
  let (_ : Thread.t) =
    Thread.create
      (fun () ->
        while true do
          let fd, _ = Unix.accept server.fd in
          let reply message =
            let m = Bytes.of_string message in
            let (_ : int) = Unix.write fd m 0 (Bytes.length m) in
            ()
          in
          finally
            (fun () ->
              let result = Bytes.make 8 '\000' in
              let n, _, received_fd =
                try Fd_send_recv.recv_fd fd result 0 (Bytes.length result) []
                with e ->
                  (* No passed fd probably means the caller doesn't realise what this socket is for. *)
                  reply error_message;
                  raise e
              in
              let actual_magic = Bytes.sub result 0 n |> Bytes.to_string in
              let ok = actual_magic = magic in
              let () =
                try reply @@ if ok then success_message else error_message
                with e ->
                  Unix.close received_fd;
                  raise e
              in
              if ok then
                Luv_lwt.in_lwt_async (fun () ->
                    Lwt.async (fun () ->
                        Sock_Dgram.of_bound_fd received_fd |> cb)))
            (fun () -> Unix.close fd)
        done)
      ()
  in
  ()

let shutdown server = run_in_pthread (fun () -> Unix.close server.fd)

let%test_unit "host_unix_dgram" =
  if Sys.os_type <> "Win32" then
    Lwt_main.run
      (let address = "/tmp/host_unix_dgram.sock" in
       (try Unix.unlink address with Unix.Unix_error (Unix.ENOENT, _, _) -> ());
       bind address >>= fun server ->
       listen server (fun flow ->
           Sock_Dgram.recv flow >>= fun buf ->
           let n = Cstruct.length buf in
           Sock_Dgram.send flow [ Cstruct.sub buf 0 n ]);
       connect address >>= fun flow ->
       let message = "hello" in
       let buf = Cstruct.create (String.length message) in
       Cstruct.blit_from_string message 0 buf 0 (String.length message);
       Sock_Dgram.send flow [ buf ] >>= fun () ->
       Sock_Dgram.recv flow >>= fun buf ->
       let n = Cstruct.length buf in
       if n <> String.length message then
         failwith
           (Printf.sprintf "n (%d) <> String.length message (%d)" n
              (String.length message));
       let response = Cstruct.to_string buf in
       if message <> response then
         failwith
           (Printf.sprintf "message (%s) <> response (%s)" message response);
       close flow)
