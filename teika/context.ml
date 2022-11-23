open Ttree

type var_info =
  | V_rigid of { level : Level.t }
  | V_alias of { var : Var.t }
  | V_link of { type_ : term_desc }
  | V_hole of { level : Level.t }

type error = CError of { loc : Location.t; desc : error_desc }

and error_desc =
  | CError_occurs_check of { var : Var.t; in_ : Var.t }
  | CError_escape_check of { var : Var.t; to_ : Var.t }
  | CError_var_constrained of { var : Var.t; by_ : term_desc }
  | CError_var_clash of { expected : Var.t; received : Var.t }
  | CError_type_clash of { expected : term_desc; received : term_desc }
  (* invariants *)
  | CError_unknown_var_repr of { var : Var.t }
  | CError_unknown_var_link of { var : Var.t }
  | CError_unknown_var_lower of { var : Var.t }
  | CError_unknown_var_alias of { var : Var.t }
  | CError_duplicated_var_rigid of { var : Var.t }
  | CError_duplicated_var_alias of { var : Var.t }
  | CError_duplicated_var_hole of { var : Var.t }
  | CError_invalid_var_link of { var : Var.t; info : var_info }
  | CError_invalid_var_lower of { var : Var.t; info : var_info }
  | CError_invalid_var_alias of { var : Var.t; info : var_info }
  | CError_lowering_to_higher_level of { var : Var.t }

type data = {
  loc : Location.t;
  (* unification *)
  level : Level.t;
  (* TODO: for variables this could be done using an array
      as even in big programs the number of variables is relatively
      small, unless we start generating variables during typing *)
  vars : var_info Var.Map.t;
}

(* TODO: I'm bad with monads *)
type 'a context = Context of (data -> ('a * data, error * data) result)
[@@ocaml.unboxed]

type 'a t = 'a context

let return content = Context (fun data -> Ok (content, data))

let fail desc =
  Context
    (fun data ->
      let { loc; level = _; vars = _ } = data in
      Error (CError { loc; desc }, data))

let apply context data f =
  let (Context context) = context in
  match context data with
  | Ok (content, data) -> f content data
  | Error _error as error -> error

let ( >>= ) context f =
  Context
    (fun data ->
      let (Context context) = context in
      match context data with
      | Ok (content, data) ->
          let (Context f) = f content in
          f data
      | Error _error as error -> error)

let fail_occurs_check var ~in_ = fail (CError_occurs_check { var; in_ })
let fail_escape_check var ~to_ = fail (CError_escape_check { var; to_ })
let fail_var_constrained var ~by_ = fail (CError_var_constrained { var; by_ })

let fail_var_clash ~expected ~received =
  fail (CError_var_clash { expected; received })

let fail_type_clash ~expected ~received =
  fail (CError_type_clash { expected; received })

let loc =
  Context
    (fun data ->
      let { loc; level = _; vars = _ } = data in
      Ok (loc, data))

let level =
  Context
    (fun data ->
      let { loc = _; level; vars = _ } = data in
      Ok (level, data))

let repr var =
  Context
    (fun data ->
      let { loc; level = _; vars } = data in
      match Var.Map.find_opt var vars with
      | Some var_info -> Ok (var_info, data)
      | None ->
          let desc = CError_unknown_var_repr { var } in
          Error (CError { loc; desc }, data))

let with_loc loc f =
  Context
    (fun data ->
      let { loc = initial_loc; level; vars } = data in
      apply (f ()) { loc; level; vars } @@ fun content data ->
      let { loc = _closing_loc; level; vars } = data in
      Ok (content, { loc = initial_loc; level; vars }))

let with_region f =
  Context
    (fun data ->
      let { loc; level = initial_level; vars } = data in
      let level = Level.next initial_level in
      apply (f ()) { loc; level; vars } @@ fun content data ->
      let { loc; level = _closing_level; vars } = data in
      (* TODO: check no variable remaining on the level before closing *)
      Ok (content, { loc; level = initial_level; vars }))

(* TODO: duplicated code *)
let with_var_rigid var f =
  Context
    (fun data ->
      let { loc; level; vars } = data in
      match Var.Map.mem var vars with
      | true ->
          let desc = CError_duplicated_var_rigid { var } in
          Error (CError { loc; desc }, data)
      | false ->
          let info = V_rigid { level } in
          let vars = Var.Map.add var info vars in
          apply (f ()) { loc; level; vars } @@ fun content data ->
          let { loc; level; vars } = data in
          (* TODO: check variable did not escape it's scope *)
          let vars = Var.Map.remove var vars in
          Ok (content, { loc; level; vars }))

(* TODO: duplicated code *)
let with_var_alias var ~of_ f =
  Context
    (fun data ->
      let { loc; level; vars } = data in
      match Var.Map.mem var vars with
      | true ->
          let desc = CError_duplicated_var_alias { var } in
          Error (CError { loc; desc }, data)
      | false ->
          let info = V_alias { var = of_ } in
          let vars = Var.Map.add var info vars in
          apply (f ()) { loc; level; vars } @@ fun content data ->
          let { loc; level; vars } = data in
          (* TODO: check variable did not escape it's scope *)
          let vars = Var.Map.remove var vars in
          Ok (content, { loc; level; vars }))

let enter_var_hole var =
  Context
    (fun data ->
      let { loc; level; vars } = data in
      match Var.Map.mem var vars with
      | true ->
          let desc = CError_duplicated_var_hole { var } in
          Error (CError { loc; desc }, data)
      | false ->
          let info = V_hole { level } in
          let vars = Var.Map.add var info vars in
          Ok ((), { loc; level; vars }))

let var_link var ~to_ =
  (* TODO: path compression *)
  Context
    (fun data ->
      let { loc; level; vars } = data in
      (* TODO: we could track substitution locations here for LSP *)
      match Var.Map.find_opt var vars with
      | Some (V_hole { level = _ }) ->
          let info = V_link { type_ = to_ } in
          let vars = Var.Map.add var info vars in
          Ok ((), { loc; level; vars })
      | Some ((V_rigid _ | V_alias _ | V_link _) as info) ->
          let desc = CError_invalid_var_link { var; info } in
          Error (CError { loc; desc }, data)
      | None ->
          let desc = CError_unknown_var_link { var } in
          Error (CError { loc; desc }, data))

let var_lower var ~to_ =
  Context
    (fun data ->
      let { loc; level; vars } = data in
      (* TODO: we could track lowering locations here for LSP *)
      match Var.Map.find_opt var vars with
      | Some (V_hole { level = hole_level }) -> (
          match Level.(hole_level < to_) with
          | true ->
              let desc = CError_lowering_to_higher_level { var } in
              Error (CError { loc; desc }, data)
          | false ->
              let info = V_hole { level = to_ } in
              let vars = Var.Map.add var info vars in
              Ok ((), { loc; level; vars }))
      | Some ((V_rigid _ | V_alias _ | V_link _) as info) ->
          let desc = CError_invalid_var_lower { var; info } in
          Error (CError { loc; desc }, data)
      | None ->
          let desc = CError_unknown_var_lower { var } in
          Error (CError { loc; desc }, data))

let var_alias var ~of_ =
  Context
    (fun data ->
      let { loc; level; vars } = data in
      (* TODO: we could track lowering locations here for LSP *)
      match Var.Map.find_opt var vars with
      | Some (V_hole { level = _ }) ->
          (* TODO: check level is equal or higher than of_ *)
          let info = V_alias { var = of_ } in
          let vars = Var.Map.add var info vars in
          Ok ((), { loc; level; vars })
      | Some ((V_rigid _ | V_alias _ | V_link _) as info) ->
          let desc = CError_invalid_var_alias { var; info } in
          Error (CError { loc; desc }, data)
      | None ->
          let desc = CError_unknown_var_alias { var } in
          Error (CError { loc; desc }, data))
