type literal = L_string of string
type t = literal

let l_string string = L_string string
let equal a b = match (a, b) with L_string a, L_string b -> String.equal a b

let pp fmt literal =
  match literal with L_string string -> Format.fprintf fmt "%S" string

let show literal = Format.asprintf "%a" pp literal
