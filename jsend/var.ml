open Teika

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

type var_kind = Global | Scoped
type var = { id : Id.t; name : Name.t; kind : var_kind }
type t = var

let pp fmt var =
  let { id; name; kind } = var in
  match kind with
  | Global -> Format.fprintf fmt "%s" (Name.repr name)
  | Scoped -> Format.fprintf fmt "%s$%a" (Name.repr name) Id.pp id

let show var = Format.asprintf "%a" pp var

let create_any kind name =
  let id = Id.next () in
  { id; name; kind }

let create name = create_any Scoped name

let predef name =
  let name = Name.make name in
  create_any Global name

let equal a b =
  let { id = a; name = _; kind = _ } = a in
  let { id = b; name = _; kind = _ } = b in
  Id.equal a b

let compare a b =
  let { id = a; name = _; kind = _ } = a in
  let { id = b; name = _; kind = _ } = b in
  Id.compare a b

let name var =
  let { id = _; name; kind = _ } = var in
  name

(* TODO: those should be checked somewhere *)
let type_ = predef "$type"
let fix = predef "$fix"
let unit = predef "$unit"
let debug = predef "$debug"
let curry = predef "$curry"

module Map = Map.Make (struct
  type t = var

  let compare = compare
end)
