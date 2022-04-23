open Utils

(* TODO: maybe ident or name should carry location *)
type t = { id : Uid.t; name : Name.t } [@@deriving show]

let make name =
  let id = Uid.next () in
  { id; name }

let name t = t.name
let equal a b = Uid.equal a.id b.id
let compare a b = Uid.compare a.id b.id

module Map = Map.Make (struct
  type nonrec t = t

  let compare = compare
end)
