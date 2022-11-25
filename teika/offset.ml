type offset = int
and t = offset [@@deriving show, eq]

let zero = 0
let one = 1
let of_int x = x
let repr x = x
let ( + ) a b = a + b
let ( - ) a b = a - b
let ( < ) (a : offset) (b : offset) = a < b
let ( > ) (a : offset) (b : offset) = a > b
