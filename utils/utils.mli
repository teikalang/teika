module Index : sig
  type index = private int
  type t = index [@@deriving show, eq]

  val zero : index
  val next : index -> index
end

module Level : sig
  (* TODO: this private int is not ideal *)
  type level = private int
  type t = level [@@deriving show, eq]

  val min : level -> level -> level
  val max : level -> level -> level
  val zero : level
  val next : level -> level
  val init : level -> (level -> 'a) -> 'a list
  val offset : from:level -> to_:level -> Index.t option

  module Map : Map.S with type key = level
end

module Name : sig
  type name
  type t = name [@@deriving show, eq, ord]

  val make : string -> name
  val repr : name -> string

  (* TODO: stop exposing this? *)
  module Map : Map.S with type key = name
end
