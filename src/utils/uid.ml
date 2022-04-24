(* TODO: 32 bits *)
assert (Sys.word_size = 64)

module type S = sig
  type t [@@deriving show]

  val next : unit -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int

  module Map : Map.S with type key = t
end

type t = int [@@deriving show]

let equal = Int.equal
let compare = Int.compare

(* TODO: discuss immutable API *)
let acc = Atomic.make 0
let next () = Atomic.fetch_and_add acc 1

module Map = Map.Make (Int)
