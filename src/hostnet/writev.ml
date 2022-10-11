type buffer = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

external stub_writev: Unix.file_descr -> (buffer * int * int) list -> int = "stub_writev"

external stub_iov_max: unit -> int = "stub_iov_max"

let iov_max = stub_iov_max ()

let writev fd bufs =
  let rec take n rev_acc = function
    | [] -> List.rev rev_acc, []
    | xs when n = 0 -> List.rev rev_acc, xs
    | x :: xs -> take (n - 1) (x :: rev_acc) xs in
  let rec loop written = function
    | [] -> written
    | remaining ->
      (* write iov_max at a time *)
      let first, rest = take iov_max [] remaining in
      let n = stub_writev fd (List.map (fun x -> x.Cstruct.buffer, x.Cstruct.off, x.Cstruct.len) first) in
      loop (written + n) rest in
  loop 0 bufs