open Utils

module M = struct
  type index = int
  type level = index

  type term =
    (* TODO: T_value? *)
    | T_hole of hole
    | T_bound_var of index
    | T_forall of { param : term; body : term }
    | T_lambda of { body : term }
    | T_apply of { lambda : term; arg : term }
    | T_let of { value : term; body : term }

  and env = value list

  and value =
    | V_hole of hole
    (* TODO: relevance marks *)
    | V_free_var of { var : level; args : value list }
    | V_forall of { param : value; body : value }
    | V_closure of { env : env; body : term }

  and hole = { mutable to_ : value; mutable tag : level }

  module Env : sig
    (* skew heap *)
    type nonrec env = env
    type t = env

    val empty : env
    val push : value -> env -> env
    val bind : env -> env
    val access : index -> env -> value
  end = struct
    type nonrec env = env
    type t = env

    let empty = []
    let push value env = value :: env
    let bind = assert false

    let rec access n env =
      match (env, n) with
      | head :: _env, 0 -> head
      | _head :: env, n -> access (n - 1) env
      | [], _n ->
          (* TODO: GADT's? *)
          failwith "invalid env access"
  end

  let same_value (left : value) (right : value) = left == right
  let same_hole (left : hole) (right : hole) = left == right
  let nil_var = 0
  let linked_var = -1
  let nil_value = V_free_var { var = nil_var; args = [] }
  let is_nil value = same_value value nil_value
  let is_linked ~tag = Int.equal tag linked_var

  let rec repr value =
    match value with
    | V_hole { to_; tag } when is_linked ~tag ->
        (* TODO: path compression, also benchmark *)
        repr to_
    | V_hole _ | V_free_var _ | V_forall _ | V_closure _ -> value

  let new_hole ~at_ =
    (* TODO: assert tag is also not generic *)
    assert (not (is_linked ~tag:at_));
    { to_ = nil_value; tag = at_ }

  let link_hole ~hole ~to_ =
    (* TODO: also ensure hole is weak and not generic? *)
    assert (not (is_linked ~tag:hole.tag));
    hole.to_ <- to_

  let would_escape ~hole ~var = hole.tag >= var

  let lower_hole ~hole ~to_ =
    (* TODO: assert to_ can be lowered *)
    to_.tag <- max to_.tag hole.tag

  let force_hole_closure ~hole =
    let body = T_hole (new_hole ~at_:hole.tag) in
    let value = V_closure { env = []; body } in
    link_hole ~hole ~to_:value;
    (`Env [], `Body body)

  let rec expand_head ~env term =
    (* TODO: gas *)
    match term with
    | T_hole hole -> V_hole hole
    | T_bound_var var ->
        (* subst *)
        Env.access var env
    | T_forall { param; body } ->
        let param = expand_head ~env param in
        let body =
          let env = Env.bind env in
          expand_head ~env body
        in
        V_forall { param; body }
    | T_lambda { body } -> V_closure { env; body }
    | T_apply { lambda; arg } -> (
        let lambda = expand_head ~env lambda in
        let arg = expand_head ~env arg in
        match lambda with
        | V_hole hole ->
            (* hole beta *)
            let `Env env, `Body body = force_hole_closure ~hole in
            (* same as closure *)
            let env = arg :: env in
            expand_head ~env body
        | V_closure { env; body } ->
            (* beta *)
            let env = Env.push arg env in
            expand_head ~env body
        | V_free_var { var; args } ->
            let args = arg :: args in
            V_free_var { var; args }
        | V_forall _ -> failwith "expand_head.apply.forall")
    | T_let { value; body } ->
        let value = expand_head ~env value in
        let env = Env.push value env in
        expand_head ~env body

  (* TODO: occurs + escape check + lowering *)
  (* TODO: lazy unify check? *)
  let rec unify_check ~hole ~to_ =
    (* TODO: color value to avoid occurs check twice? *)
    (* TODO: short circuit with levels on value *)
    match repr to_ with
    | V_hole to_ ->
        (match same_hole hole to_ with
        | true -> failwith "occurs check"
        | false -> ());
        (* lower *)
        lower_hole ~hole ~to_
    | V_free_var { var; args } ->
        (match would_escape ~hole ~var with
        | true -> failwith "escape check"
        | false -> ());
        List.iter (fun arg -> unify_check ~hole ~to_:arg) args
    | V_forall { param; body } ->
        unify_check ~hole ~to_:param;
        unify_check ~hole ~to_:body
    | V_closure { env; body } ->
        (* TODO: this could be done faster, free weakening? *)
        let body =
          let env = Env.bind env in
          expand_head ~env body
        in
        unify_check ~hole ~to_:body

  let unify_hole ~hole ~to_ =
    match to_ with
    | V_hole to_ when same_hole hole to_ -> ()
    | V_hole _ | V_free_var _ | V_forall _ | V_closure _ ->
        unify_check ~hole ~to_;
        hole.to_ <- to_

  let rec unify ~left ~right =
    (* TODO: phys equality *)
    match same_value left right with
    | true -> ()
    | false -> unify_struct ~left ~right

  and unify_struct ~left ~right =
    match (repr left, repr right) with
    | V_hole hole, to_ -> unify_hole ~hole ~to_
    | to_, V_hole hole -> unify_hole ~hole ~to_
    | ( V_free_var { var = left; args = left_args },
        V_free_var { var = right; args = right_args } ) ->
        (* TODO: var clash *)
        (match Int.equal left right with
        | true -> ()
        | false -> failwith "var clash");
        unify_args ~left_args ~right_args
    | ( V_forall { param = left_param; body = left_body },
        V_forall { param = right_param; body = right_body } ) ->
        (* TODO: contravariance? *)
        unify ~left:left_param ~right:right_param;
        unify ~left:left_body ~right:right_body
    | ( V_closure { env = left_env; body = left },
        V_closure { env = right_env; body = right } ) ->
        (* TODO: eta *)
        let left =
          let env = Env.bind left_env in
          expand_head ~env left
        in
        let right =
          let env = Env.bind right_env in
          expand_head ~env right
        in
        unify ~left ~right
    | V_free_var _, (V_forall _ | V_closure _)
    | V_closure _, (V_free_var _ | V_forall _)
    | V_forall _, (V_free_var _ | V_closure _) ->
        failwith "type clash"

  and unify_args ~left_args ~right_args =
    match (left_args, right_args) with
    | left :: left_args, right :: right_args ->
        unify ~left ~right;
        unify_args ~left_args ~right_args
    | [], [] -> ()
    | _, _ -> assert false
end

type term =
  (* #(M : A) *)
  | TTerm of { term : term_syntax; mutable type_ : term }
  (* #(A : S) *)
  | TType of { term : term_syntax }

and term_syntax =
  (* (M : A) *)
  | TT_annot of { term : term; annot : term }
  (* \+n *)
  | TT_free_var of { var : Level.t }
  (* \-n *)
  | TT_bound_var of { var : Index.t }
  (* P -> B *)
  | TT_forall of { param : pat; return : term }
  (* P => M *)
  | TT_lambda of { param : pat; return : term }
  (* M N *)
  | TT_apply of { lambda : term; arg : term }
  (* P = N; M *)
  | TT_let of { bound : pat; value : term; return : term }
  (* ".." *)
  | TT_string of { literal : string }

and pat = (* #(P : A) *)
  | TPat of { pat : pat_syntax; mutable type_ : term }

and pat_syntax =
  (* (P : A) *)
  | TP_annot of { pat : pat; annot : term }
  (* x *)
  | TP_var of { name : Name.t }
[@@deriving show { with_path = false }]

(* TODO: expose this? *)
(* terms *)
let tterm ~type_ term = TTerm { term; type_ }
let ttype term = TType { term }
let tt_annot ~term ~annot = TT_annot { term; annot }
let tt_free_var ~var = TT_free_var { var }
let tt_bound_var ~var = TT_bound_var { var }
let tt_forall ~param ~return = TT_forall { param; return }
let tt_lambda ~param ~return = TT_lambda { param; return }
let tt_apply ~lambda ~arg = TT_apply { lambda; arg }
let tt_let ~bound ~value ~return = TT_let { bound; value; return }
let tt_string ~literal = TT_string { literal }

(* patterns *)
let tpat ~type_ pat = TPat { pat; type_ }
let tp_annot ~pat ~annot = TP_annot { pat; annot }
let tp_var ~name = TP_var { name }

(* Nil *)
let level_nil = Level.zero
let tterm_nil = ttype @@ TT_free_var { var = level_nil }
let tpat_nil = tpat ~type_:tterm_nil @@ TP_var { name = Name.make "**nil**" }

(* Type *)
let level_univ = Level.next level_nil
let tt_type_univ = ttype @@ TT_free_var { var = level_univ }

(* String *)
let level_string = Level.next level_univ
let tt_type_string = ttype @@ TT_free_var { var = level_string }
