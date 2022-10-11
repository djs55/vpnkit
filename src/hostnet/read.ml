type buffer = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

external stub_cstruct_read: Unix.file_descr -> buffer -> int -> int -> int = "stub_cstruct_read"

let read fd c = stub_cstruct_read fd c.Cstruct.buffer c.Cstruct.off c.Cstruct.len