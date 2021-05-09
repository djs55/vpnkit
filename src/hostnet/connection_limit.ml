
let src =
  let src = Logs.Src.create "Connection_limit" ~doc:"Track and limit open connections" in
  Logs.Src.set_level src (Some Logs.Info);
  src

module Log = (val Logs.src_log src : Logs.LOG)

let max = ref None

let set_max x =
  begin match x with
    | None -> Log.info (fun f -> f "Removed connection limit")
    | Some limit -> Log.info (fun f -> f "Updated connection limit to %d" limit)
  end;
  max := x

let next_connection_idx =
  let idx = ref 0 in
  fun () ->
    let next = !idx in
    incr idx;
    next

    let connection_table = Hashtbl.create 511

let get_num_connections () = Hashtbl.length connection_table

let connections () =
  let xs = Hashtbl.fold (fun _ c acc -> c :: acc) connection_table [] in
  Vfs.File.ro_of_string (String.concat "\n" xs)

let register_no_limit description =
  let idx = next_connection_idx () in
  Hashtbl.replace connection_table idx description;
  idx

let register =
  let last_error_log = ref 0. in
  fun description -> match !max with
  | Some m when Hashtbl.length connection_table >= m ->
    let now = Unix.gettimeofday () in
    if (now -. !last_error_log) > 30. then begin
      (* Avoid hammering the logging system *)
      Log.warn (fun f ->
          f "Exceeded maximum number of forwarded connections (%d)" m);
      last_error_log := now;
    end;
    Error (`Msg "too many open connections")
  | _ ->
    Ok (register_no_limit description)

let deregister idx =
  if not(Hashtbl.mem connection_table idx) then begin
    Log.warn (fun f -> f "Deregistered connection %d more than once" idx)
  end;
  Hashtbl.remove connection_table idx