type forall
type t = forall [@@deriving show]

(* TODO: what about recursive types? Mutation seems to be bad with those *)
val same : t -> t -> bool

(* TODO: why forall and rank coupling? *)
val rank : t -> Rank.t
val is_generic : t -> bool
val weak : Rank.t -> t
val generic : unit -> t
val universal : t -> unit
val existential : t -> unit
val with_rank : (unit -> 'a) -> Rank.t -> t -> 'a
