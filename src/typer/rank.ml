type t = int [@@deriving show]

let initial = 0
let next t = t + 1
let ( > ) (a : int) (b : int) = a > b
let ( >= ) (a : int) (b : int) = a >= b
