type t = string

let make value = value
let pp formatter value = Format.pp_print_string formatter value
let equal = String.equal
