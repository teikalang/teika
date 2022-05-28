type forall
type t = forall [@@deriving show]

(* TODO: what about recursive types? Mutation seems to be bad with those *)
val same : t -> t -> bool
val make : unit -> t

(* TODO: can be optimized by making a special Rank.generic *)
val rank : t -> Rank.t option
val tag : Rank.t -> t -> unit
val clear : t -> unit
