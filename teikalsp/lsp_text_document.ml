(* TODO: proper types for text and version *)
type document = Smol of { version : int; text : string }
type t = document

let teika ~version ~text = Smol { version; text }

let with_change ~version ~text document =
  (* TODO: use the version for something? *)
  let (Smol { version = _; text = _ }) = document in
  Smol { version; text }
