type offset = int
and t = offset [@@deriving show, eq]

let zero = 0
let one = 1
let of_int x = x
let repr x = x
let predef offset = (offset + one, offset)
let initial = zero
let initial, type_ = predef initial
let ( + ) a b = a + b
let ( - ) a b = a - b
let ( < ) (a : offset) (b : offset) = a < b
