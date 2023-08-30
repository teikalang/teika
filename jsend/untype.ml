open Teika
open Ttree
open Utree
open Expand_head

(* TODO: Type will lead to issues,
   it will be a free var *)
exception Free_var_found
exception Term_hole_found
exception Pat_hole_found
exception Invalid_variable

let type_term : term = UT_external { external_ = UE_type }
let fix_term : term = UT_external { external_ = UE_fix }
let unit_term : term = UT_external { external_ = UE_unit }
let debug_term : term = UT_external { external_ = UE_debug }

module Context : sig
  type 'a context

  val run : (unit -> 'a context) -> 'a
  val return : 'a -> 'a context
  val ( let* ) : 'a context -> ('a -> 'b context) -> 'b context
  val ( let+ ) : 'a context -> ('a -> 'b) -> 'b context
  val with_var : Name.t -> (Var.t -> 'k context) -> 'k context
  val lookup : Index.t -> Var.t context
end = struct
  type 'a context = vars:Var.t list -> 'a

  let run context = context () ~vars:[]
  let return x ~vars:_ = x
  let ( let* ) context k ~vars = k (context ~vars) ~vars
  let ( let+ ) context k ~vars = k (context ~vars)

  let with_var name k ~vars =
    let var = Var.create name in
    let vars = var :: vars in
    k var ~vars

  let lookup index ~vars =
    match List.nth_opt vars (Index.repr index) with
    | Some var -> var
    | None -> raise Invalid_variable
end

open Context

let rec untype_term : type a. a Ttree.term -> term context =
 fun term ->
  match expand_head_term term with
  | TT_bound_var { index } ->
      let+ var = lookup index in
      UT_var { var }
  (* TODO: those should definitely not be hard coded here  *)
  | TT_free_var { level; alias = None } when Level.equal level type_level ->
      let var = Var.type_ in
      return @@ UT_var { var }
  | TT_free_var { level; alias = None } when Level.equal level string_level ->
      (* TODO: is it okay for string to be this? *)
      let var = Var.type_ in
      return @@ UT_var { var }
  | TT_free_var _ -> raise Free_var_found
  | TT_hole _ -> raise Term_hole_found
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
  | TT_self _ -> return type_term
  | TT_fix { var; body } ->
      let+ var, body =
        erase_pat var @@ fun var ->
        let+ body = untype_term body in
        (var, body)
      in
      let constructor = UT_lambda { param = var; return = body } in
      UT_apply { lambda = fix_term; arg = constructor }
  | TT_unroll { term } ->
      let+ term = untype_term term in
      UT_apply { lambda = term; arg = unit_term }
  | TT_string { literal } -> return @@ UT_string { literal }
  | TT_native { native } -> erase_native native

and erase_native native = match native with TN_debug -> return @@ debug_term

and erase_pat : type a k. a Ttree.pat -> (Var.t -> k context) -> k context =
 fun pat k ->
  match pat with
  | TP_typed { pat; annot = _ } -> erase_pat pat k
  | TP_hole _ -> raise Pat_hole_found
  | TP_var { name } -> with_var name k

let untype_term term = Context.run @@ fun () -> untype_term term
