type forall = { mutable rank : Rank.t } [@@deriving show { with_path = false }]
type t = forall [@@deriving show]

let same a b = a == b
let make rank = { rank }

let rank t =
  let { rank } = t in
  rank

let bind t = t.rank <- Rank.generic

let with_rank f rank t =
  let previous_rank = t.rank in
  t.rank <- rank;
  let value = f () in
  t.rank <- previous_rank;
  value
