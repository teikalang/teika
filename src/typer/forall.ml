type forall = { mutable rank : Rank.t option }
[@@deriving show { with_path = false }]

type t = forall [@@deriving show]

let same a b = a == b
let make () = { rank = None }

let rank t =
  let { rank } = t in
  rank

let tag rank t = t.rank <- Some rank
let clear t = t.rank <- None
