open Syntax
open Teika
open Ttree
open Utree

exception Term_subst_found
exception Term_shift_found
exception Invalid_variable

let type_term : term = UT_external { external_ = UE_type }
(* let fix_term : term = UT_external { external_ = UE_fix }
   let unit_term : term = UT_external { external_ = UE_unit }
   let debug_term : term = UT_external { external_ = UE_debug } *)

let next_level ~vars =
  let current_level =
    match Level.Map.max_binding_opt vars with
    | Some (current_level, _) -> current_level
    | None ->
        (* TODO: this is weird *)
        level_type_string
  in
  Level.next current_level

module Context : sig
  type 'a context

  val run : (unit -> 'a context) -> 'a
  val return : 'a -> 'a context
  val ( let* ) : 'a context -> ('a -> 'b context) -> 'b context
  val ( let+ ) : 'a context -> ('a -> 'b) -> 'b context
  val with_var : Name.t -> (Var.t -> 'k context) -> 'k context
  val lookup : Level.t -> Var.t context
end = struct
  type 'a context = vars:Var.t Level.Map.t -> 'a

  let run context =
    let vars =
      let open Level.Map in
      let vars = empty in
      (* TODO: string is also a $type *)
      let vars = add level_type_univ Var.type_ vars in
      let vars = add level_type_string Var.type_ vars in
      vars
    in
    context () ~vars

  let return x ~vars:_ = x
  let ( let* ) context k ~vars = k (context ~vars) ~vars
  let ( let+ ) context k ~vars = k (context ~vars)

  let with_var name k ~vars =
    let var = Var.create name in
    let level = next_level ~vars in
    let vars = Level.Map.add level var vars in
    k var ~vars

  let lookup level ~vars =
    match Level.Map.find_opt level vars with
    | Some var -> var
    | None -> raise Invalid_variable
end

open Context

let rec untype_term term =
  match term with
  | TT_with_type { term; type_ = _ } -> untype_term term
  | TT_with_sort { term } ->
      (* TODO: should also not be reachable? *)
      untype_term term
  | TT_subst _ -> raise Term_subst_found
  | TT_shift _ -> raise Term_shift_found
  | TT_var { var } ->
      let+ var = lookup var in
      UT_var { var }
  | TT_forall _ -> return type_term
  | TT_lambda { param; return } ->
      let+ param, return =
        erase_pat param @@ fun var ->
        let+ return = untype_term return in
        (var, return)
      in
      UT_lambda { param; return }
  | TT_apply { lambda; arg } ->
      let* lambda = untype_term lambda in
      let+ arg = untype_term arg in
      UT_apply { lambda; arg }
  | TT_let { bound; value; return } ->
      (* TODO: param first *)
      let* value = untype_term value in
      let+ var, return =
        erase_pat bound @@ fun var ->
        let+ return = untype_term return in
        (var, return)
      in
      UT_let { var; value; return }
  | TT_annot { term; annot = _ } -> untype_term term
  | TT_string { literal } -> return @@ UT_string { literal }

and erase_pat pat k =
  match pat with
  | TP_with_type { pat; type_ = _ } -> erase_pat pat k
  | TP_annot { pat; annot = _ } -> erase_pat pat k
  | TP_var { name } -> with_var name k

let untype_term term = Context.run @@ fun () -> untype_term term
