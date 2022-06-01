type forall = {
  mutable rank : Rank.t;
  (* TODO: generic could be inside of rank *)
  mutable generic : bool;
}
[@@deriving show { with_path = false }]

type t = forall [@@deriving show]

let same a b = a == b
let weak rank = { rank; generic = false }
let generic () = { rank = Rank.generic; generic = true }

let rank t =
  let { rank; generic = _ } = t in
  rank

let is_generic t =
  let { rank = _; generic } = t in
  generic

let universal t =
  t.rank <- Rank.generic;
  t.generic <- true

let existential t =
  t.rank <- Rank.generic;
  t.generic <- true

let with_rank f rank t =
  let previous_rank = t.rank in
  t.rank <- rank;
  let value = f () in
  t.rank <- previous_rank;
  value
