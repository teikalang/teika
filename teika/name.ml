type t = string [@@deriving show, eq, ord]

let make t = t
let repr t = t

module Map = Map.Make (String)
