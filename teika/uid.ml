type t = int [@@deriving show, eq, ord]

let initial = 0
let next n = n + 1
let repr n = n
