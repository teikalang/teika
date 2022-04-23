type t = string [@@deriving show]

let make t = t
let equal = String.equal
let compare = String.compare
let repr t = t

module Map = Map.Make (String)
