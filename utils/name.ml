type name = string
and t = name [@@deriving show, eq, ord]

let make t = t
let repr t = t

module Map = Map.Make (String)
