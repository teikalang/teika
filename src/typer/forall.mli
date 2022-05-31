type forall
type t = forall [@@deriving show]

(* TODO: what about recursive types? Mutation seems to be bad with those *)
val same : t -> t -> bool
val rank : t -> Rank.t
val make : Rank.t -> t
val bind : t -> unit
val with_rank : (unit -> 'a) -> Rank.t -> t -> 'a
