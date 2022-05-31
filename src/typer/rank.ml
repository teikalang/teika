type t = int [@@deriving show]

let generic = 0
let initial = 1
let next t = t + 1
let ( > ) (a : int) (b : int) = a > b
let ( >= ) (a : int) (b : int) = a >= b
