module type S = sig
  type t [@@deriving show]

  val next : unit -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int

  module Map : Map.S with type key = t
end

include S
