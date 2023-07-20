type index = int
and t = index [@@deriving show, eq]

let zero = 0
let one = 1
let of_int x = x
let repr x = x
let ( + ) a b = a + b
let ( - ) a b = a - b
let ( < ) (a : index) (b : index) = a < b
let ( > ) (a : index) (b : index) = a > b
