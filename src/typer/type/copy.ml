module Mem_map : sig
  type ('k, 'v) t

  val empty : ('k, 'v) t
  val add : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
  val find : 'k -> ('k, 'v) t -> 'v option
end = struct
  type ('k, 'v) t = ('k * 'v) list

  let empty = []
  let add k v t = (k, v) :: t
  let find k t = List.assq_opt k t
end

type t = { mutable desc : desc; mutable data : t }
and desc = Leaf of int | Node of t * t

let new_t desc =
  let rec t = { desc; data = t } in
  t

let with_leaf t n =
  t.desc <- Leaf n;
  t

let with_node t l r =
  t.desc <- Node (l, r);
  t

let rec clear t =
  if t.data == t then () else t.data <- t;
  clear_desc t.desc

and clear_desc desc =
  match desc with
  | Leaf _ -> ()
  | Node (l, r) ->
      clear l;
      clear r

let rec copy t =
  if not (t.data == t) then t.data
  else
    let desc = t.desc in
    let to_ = new_t desc in
    t.data <- to_;
    copy_desc desc ~to_

and copy_desc desc ~to_ =
  match desc with
  | Leaf n -> with_leaf to_ n
  | Node (l, r) ->
      let l = copy l in
      let r = copy r in
      with_node to_ l r

let copy t =
  let t' = copy t in
  clear t;
  t'
