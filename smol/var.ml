module Id : sig
  type t [@@deriving show]

  val next : unit -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int
end = struct
  type t = int [@@deriving show]

  let acc = Atomic.make 0
  let next () = Atomic.fetch_and_add acc 1
  let equal = Int.equal
  let compare = Int.compare
end

let _ = Id.show

type var = { id : Id.t; name : Name.t } [@@deriving show]
type t = var [@@deriving show]

let create name =
  let id = Id.next () in
  { id; name }

let equal a b =
  let { id = a; name = _ } = a in
  let { id = b; name = _ } = b in
  Id.equal a b

let compare a b =
  let { id = a; name = _ } = a in
  let { id = b; name = _ } = b in
  Id.compare a b

let name var =
  let { id = _; name } = var in
  name
