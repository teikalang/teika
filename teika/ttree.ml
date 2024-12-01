open Utils

type var

type block =
  | B_let of { arg : code; next : block }
  | B_block of { arg : block; next : block }
  | B_return of { code : code }

and code =
  | C_var of { var : var }
  | C_apply of { funct : var; arg : code }
  | C_lambda of { body : block }

type term = Term of { struct_ : term_struct; loc : Location.t [@opaque] }

and term_struct =
  (* _A : Type *)
  | T_hole of { hole : value }
  (* (M : A) *)
  | T_annot of { term : term; annot : term }
  (* \n *)
  | T_var of { var : Index.t }
  (* P = N; M *)
  | T_let of { bound : pat; arg : term; body : term }
  (* P : A; M *)
  | T_hoist of { bound : pat; body : term }
  (* P : A; ...; P = N; M *)
  (* TODO: pattern on fix? *)
  | T_fix of { bound : pat; var : Index.t; arg : term; body : term }
  (* P => M *)
  | T_lambda of { param : pat; body : term }
  (* M N *)
  | T_apply of { funct : term; arg : term }
  (* (P : A) -> B *)
  | T_forall of { param : pat; body : term }
  (* TODO: part of fix *)
  (* (P : A) & B *)
  | T_self of { self : pat; body : term }

and pat =
  | Pat of {
      struct_ : pat_struct;
      mutable annot : term;
      loc : Location.t; [@opaque]
    }

and pat_struct =
  (* x *)
  (* TODO: drop names and uses receipts *)
  | P_var of { var : Name.t }

and value = { mutable struct_ : value_struct; mutable at : Level.t }

and value_struct =
  | V_hole
  | V_var of { name : Name.t }
  | V_forward of { mutable inner : value [@opaque] }
  | V_apply of { funct : value; arg : value }
  | V_lambda of { param : value_pat; env : env; [@opaque] body : term }
    (* TODO: is univ actually needed or useful here? *)
  | V_univ
  (* TODO: non dependent version of types *)
  | V_forall of { param : value_pat; env : env; [@opaque] body : term }
  | V_self of { self : value_pat; env : env; [@opaque] body : term }
  | V_thunk of { env : env; [@opaque] term : term }
  | V_link of { mutable value : value }

and value_pat = VPat of { name : Name.t; type_ : value }
and env = value list [@@deriving show { with_path = false }]

let t_null = Term { struct_ = T_var { var = Index.zero }; loc = Location.none }
let same_term (left : term) (right : term) = left == right
let t_wrap ~loc struct_ = Term { struct_; loc }
let p_wrap ~loc ~annot struct_ = Pat { struct_; annot; loc }
let v_new ~at struct_ = { struct_; at }

let v_null =
  let name = Name.make "**null**" in
  v_new ~at:Level.zero @@ V_var { name }

let v_var ~at ~name = v_new ~at @@ V_var { name }
let fresh_v_hole ~at = v_new ~at @@ V_hole

let fresh_v_forward () =
  (* TODO: proper level here *)
  let at = Level.zero in
  v_new ~at @@ V_forward { inner = v_null }

let p_init_hole ~at pat =
  (* TODO: still weird *)
  let (Pat ({ struct_ = _; annot; loc } as pat)) = pat in
  match same_term t_null annot with
  | true ->
      let hole = fresh_v_hole ~at in
      pat.annot <- t_wrap ~loc @@ T_hole { hole };
      hole
  | false -> failwith "p_init_hole: not a null annotation"

let v_apply ~funct ~arg =
  let at = Level.max funct.at arg.at in
  v_new ~at @@ V_apply { funct; arg }

let v_lambda ~param ~env ~body =
  (* TODO: proper level for lambdas *)
  let at = Level.zero in
  v_new ~at @@ V_lambda { param; env; body }

let v_univ = v_new ~at:Level.zero @@ V_univ

let v_forall ~param ~env ~body =
  (* TODO: proper level for forall *)
  let at = Level.zero in
  v_new ~at @@ V_forall { param; env; body }

let v_self ~self ~env ~body =
  (* TODO: proper level for self *)
  let at = Level.zero in
  v_new ~at @@ V_self { self; env; body }

let v_thunk ~env ~term =
  (* TODO: proper level here *)
  let at = Level.zero in
  v_new ~at @@ V_thunk { env; term }

let rec repr value =
  match value.struct_ with
  | V_link { value } -> repr value
  | V_hole | V_var _ | V_forward _ | V_apply _ | V_lambda _ | V_univ
  | V_forall _ | V_self _ | V_thunk _ ->
      value

(* TODO: inline repr? *)
let repr value =
  match value.struct_ with
  | V_link ({ value } as link) ->
      (* path compression *)
      let value = repr value in
      link.value <- value;
      value
  | V_hole | V_var _ | V_forward _ | V_apply _ | V_lambda _ | V_univ
  | V_forall _ | V_self _ | V_thunk _ ->
      value

let struct_ value = (repr value).struct_

(* TODO: level vs at *)
let level value = (repr value).at
let same (left : value) (right : value) = left == right

let init_forward value ~to_ =
  let value = repr value in
  match value.struct_ with
  | V_forward ({ inner } as forward) -> (
      match same inner v_null with
      | true -> forward.inner <- to_
      | false -> failwith "init_forward: already initialized")
  | V_hole | V_var _ | V_apply _ | V_lambda _ | V_univ | V_forall _ | V_self _
  | V_thunk _ | V_link _ ->
      failwith "init_forward: not a forward"

let lock_forward value f =
  match struct_ value with
  | V_forward ({ inner } as forward) ->
      forward.inner <- v_null;
      let finally () = forward.inner <- inner in
      Fun.protect ~finally f
  | V_hole | V_var _ | V_apply _ | V_lambda _ | V_univ | V_forall _ | V_self _
  | V_thunk _ | V_link _ ->
      assert false

let hole_lower hole ~to_ =
  let hole = repr hole in
  (match hole.struct_ with
  | V_hole -> ()
  | _ -> failwith "hole_lower: not a hole");
  hole.at <- Level.min hole.at to_

let hole_link hole ~to_ =
  let hole = repr hole in
  (match hole.struct_ with
  | V_hole -> ()
  | _ -> failwith "link_hole: not a hole");
  hole.struct_ <- V_link { value = to_ }

let thunk_link thunk ~to_ =
  let thunk = repr thunk in
  (match thunk.struct_ with
  | V_thunk _ -> ()
  | _ -> failwith "link_thunk: not a thunk");
  thunk.struct_ <- V_link { value = to_ }

let empty = []

let access env var =
  let var = (var : Index.t :> int) in
  match List.nth_opt env var with
  | Some value -> value
  | None -> failwith "lookup: unknown variable"

let append env value = value :: env
