let src =
  let src =
    Logs.Src.create "host_unix_stream"
      ~doc:"Host Unix SOCK_STREAM implementation"
  in
  Logs.Src.set_level src (Some Logs.Info);
  src

module Log = (val Logs.src_log src : Logs.LOG)

module Sock_Stream = UnixThreadsIO.Make (struct
  let send = Writev.writev
  let recv = Read.read
end)

type flow = Sock_Stream.flow
type error = [ `Closed | `Msg of string ]

let pp_error ppf = function
  | `Closed -> Fmt.string ppf "Closed"
  | `Msg m -> Fmt.string ppf m

type write_error = error

let pp_write_error = pp_error

open Lwt.Infix

let read t =
  Sock_Stream.recv t >>= fun buf ->
  let n = Cstruct.length buf in
  if n = 0 then Lwt.return @@ Ok `Eof else Lwt.return @@ Ok (`Data buf)

let write t buf =
  Lwt.catch
    (fun () -> Sock_Stream.send t [ buf ] >>= fun () -> Lwt.return @@ Ok ())
    (fun e -> Lwt.return (Error (`Msg (Printexc.to_string e))))

let writev t bufs =
  Lwt.catch
    (fun () -> Sock_Stream.send t bufs >>= fun () -> Lwt.return @@ Ok ())
    (fun e -> Lwt.return (Error (`Msg (Printexc.to_string e))))

let close t =
  Sock_Stream.close t;
  Lwt.return_unit

type address = string

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

let connect ?read_buffer_size:_ address =
  let open Lwt.Infix in
  run_in_pthread (fun () ->
      let s = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
      try
        Unix.connect s (Unix.ADDR_UNIX address);
        Ok s
      with e ->
        Unix.close s;
        Error e)
  >>= function
  | Ok fd -> Lwt.return (Ok (Sock_Stream.of_bound_fd fd))
  | Error e -> Lwt.return (Error (`Msg (Printexc.to_string e)))

let shutdown_write t = Sock_Stream.shutdown_write t; Lwt.return_unit
let shutdown_read t = Sock_Stream.shutdown_read t; Lwt.return_unit
