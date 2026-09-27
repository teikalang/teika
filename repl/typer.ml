module Name = Ctree.Name
module Syntax_tree = Lparser.Syntax_tree

module Level : sig
  type level
  type t = level

  val ( <= ) : level -> level -> bool
  val ( < ) : level -> level -> bool
  val zero : level
  val succ : level -> level
  val min : level -> level -> level
  val max : level -> level -> level
end = struct
  type level = int
  type t = level

  let ( <= ) (l : level) (r : level) : bool = l <= r
  let ( < ) (l : level) (r : level) : bool = l < r
  let zero = Int.zero
  let succ = Int.succ
  let min = Int.min
  let max = Int.max
end

exception Error of Location.t * string

let error loc message = raise (Error (loc, message))

module Typed_tree = struct
  type term =
    | Term of {
        mutable link : term;
        head : term_head;
        mutable at : Level.t;
        mutable mark : int;
        loc : Location.t;
      }

  and term_head =
    | T_flex
    | T_type
    | T_var of { var : var }
    | T_let of { bound : var; arg : term; body : term }
    | T_pi of { param : term; body : term }
    | T_lambda of { bound : var; body : term }
    | T_apply of { funct : term; arg : term }
    | T_self of { body : term }
    | T_fix of { body : term }

  and var =
    | Var of {
        to_ : term option;
        at : Level.t;
        name : Name.t;
        loc : Location.t;
      }

  let t_wrap loc ~at head =
    let rec self = Term { link = self; head; at; mark = 0; loc } in
    self

  let v_same (left : var) (right : var) = left == right
  let v_fresh loc ?to_ ~name ~at () = Var { to_; at; name; loc }
  let v_at (Var { at; _ }) = at
  let v_name (Var { name; _ }) = name
  let v_loc (Var { loc; _ }) = loc
  let t_raw_same (left : term) (right : term) = left == right
  let t_raw_link (Term inner) ~to_ = inner.link <- to_

  let rec t_repr term =
    let (Term { link; _ }) = term in
    match t_raw_same link term with
    | true -> term
    | false ->
        let final = t_repr link in
        let () = t_raw_link term ~to_:final in
        final

  let t_same left right = t_raw_same (t_repr left) (t_repr right)

  let t_at term =
    let (Term { at; _ }) = t_repr term in
    at

  let t_head term =
    let (Term { head; _ }) = t_repr term in
    head

  let t_loc term =
    let (Term { loc; _ }) = t_repr term in
    loc

  let t_link term ~to_ = t_raw_link (t_repr term) ~to_:(t_repr to_)

  let t_lower term ~to_ =
    let (Term inner) = t_repr term in
    inner.at <- Level.min inner.at to_

  let t_type loc = t_wrap loc ~at:Level.zero T_type
  let t_flex loc ~at () = t_wrap loc ~at T_flex
  let t_var loc var = t_wrap loc ~at:(v_at var) (T_var { var })

  let t_pi loc param body =
    let at = Level.max (t_at param) (t_at body) in
    t_wrap loc ~at (T_pi { param; body })

  let t_lambda loc bound body =
    t_wrap loc ~at:(t_at body) (T_lambda { bound; body })

  let t_apply loc funct arg =
    let at = Level.max (t_at funct) (t_at arg) in
    t_wrap loc ~at (T_apply { funct; arg })

  let t_self loc body = t_wrap loc ~at:(t_at body) (T_self { body })
  let t_fix loc body = t_wrap loc ~at:(t_at body) (T_fix { body })

  let t_let loc bound arg body =
    let at = Level.max (t_at arg) (t_at body) in
    t_wrap loc ~at (T_let { bound; arg; body })

  (* A new command gets its own graph. The maps preserve sharing and binder
     identity without allowing its unification to change an earlier session. *)
  let rec copy_term (terms, vars) term =
    let term = t_repr term in
    match List.find_opt (fun (found, _) -> t_raw_same found term) terms with
    | Some (_, copied) -> (copied, (terms, vars))
    | None ->
        let state = (terms, vars) in
        let head, state =
          match t_head term with
          | T_flex -> (T_flex, state)
          | T_type -> (T_type, state)
          | T_var { var } ->
              let var, state = copy_var state var in
              (T_var { var }, state)
          | T_let { bound; arg; body } ->
              let bound, state = copy_var state bound in
              let arg, state = copy_term state arg in
              let body, state = copy_term state body in
              (T_let { bound; arg; body }, state)
          | T_pi { param; body } ->
              let param, state = copy_term state param in
              let body, state = copy_term state body in
              (T_pi { param; body }, state)
          | T_lambda { bound; body } ->
              let bound, state = copy_var state bound in
              let body, state = copy_term state body in
              (T_lambda { bound; body }, state)
          | T_apply { funct; arg } ->
              let funct, state = copy_term state funct in
              let arg, state = copy_term state arg in
              (T_apply { funct; arg }, state)
          | T_self { body } ->
              let body, state = copy_term state body in
              (T_self { body }, state)
          | T_fix { body } ->
              let body, state = copy_term state body in
              (T_fix { body }, state)
        in
        let copied = t_wrap (t_loc term) ~at:(t_at term) head in
        let terms, vars = state in
        (copied, ((term, copied) :: terms, vars))

  and copy_var (terms, vars) (Var { to_; at; name; loc } as var) =
    match List.find_opt (fun (found, _) -> v_same found var) vars with
    | Some (_, copied) -> (copied, (terms, vars))
    | None ->
        let to_, (terms, vars) =
          match to_ with
          | None -> (None, (terms, vars))
          | Some value ->
              let value, state = copy_term (terms, vars) value in
              (Some value, state)
        in
        let copied = v_fresh loc ?to_ ~at ~name () in
        (copied, (terms, (var, copied) :: vars))

  let subst var to_ term =
    let rec sub replacements term =
      let (Term { head; loc; _ }) = t_repr term in
      let apply = sub replacements in
      match head with
      | T_flex | T_type -> term
      | T_var { var = found } -> (
          match
            List.find_opt (fun (var, _) -> v_same found var) replacements
          with
          | Some (_, value) -> value
          | None -> (
              let (Var { to_; _ }) = found in
              match to_ with Some value -> apply value | None -> term))
      | T_let { bound; arg; body } ->
          sub ((bound, apply arg) :: replacements) body
      | T_pi { param; body } -> t_pi loc (apply param) (apply body)
      | T_lambda { bound; body } ->
          let replacements =
            List.filter (fun (var, _) -> not (v_same bound var)) replacements
          in
          if replacements = [] then term
          else
            let fresh =
              v_fresh (v_loc bound) ~name:(v_name bound) ~at:(v_at bound) ()
            in
            (* Freshen and substitute together: traversing the body separately
               for each operation doubles the work at every nested binder. *)
            let replacements =
              (bound, t_var (v_loc bound) fresh) :: replacements
            in
            t_lambda loc fresh (sub replacements body)
      | T_apply { funct; arg } -> t_apply loc (apply funct) (apply arg)
      | T_self { body } -> t_self loc (apply body)
      | T_fix { body } -> t_fix loc (apply body)
    in
    sub [ (var, to_) ] term

  (* Reduction state is passed explicitly, independently for each operation. *)
  let reduction_limit = 10000

  let rec reduce fuel active term =
    let term = t_repr term in
    let loc = t_loc term in
    if fuel = 0 then error loc "reduction limit exceeded";
    let same_reduction other =
      t_same term other
      ||
      match (t_head term, t_head other) with
      | T_apply { funct; arg }, T_apply { funct = other; arg = other_arg } ->
          t_same funct other && t_same arg other_arg
      | _ -> false
    in
    if List.exists same_reduction active then error loc "cyclic reduction";
    let active = term :: active in
    let fuel = fuel - 1 in
    match t_head term with
    | T_var { var = Var { to_ = Some value; _ } } -> reduce fuel active value
    | T_let { bound; arg; body } -> reduce fuel active (subst bound arg body)
    | T_apply { funct; arg } -> (
        let funct, fuel = reduce fuel active funct in
        match t_head funct with
        | T_lambda { bound; body } -> reduce fuel active (subst bound arg body)
        | T_fix { body } ->
            let unfolded = t_apply loc body funct in
            let applied = t_apply loc unfolded arg in
            reduce fuel active applied
        | _ -> (t_apply loc funct arg, fuel))
    | _ -> (term, fuel)

  let whnf term = fst (reduce reduction_limit [] term)

  let expand term =
    let to_ = whnf term in
    t_link term ~to_

  let apply loc funct arg = whnf (t_apply loc funct arg)

  let unify_check ~flex term =
    let rec check bound seen term =
      let term = whnf term in
      if t_same flex term then error (t_loc term) "unify: occurs check";
      if List.exists (t_same term) seen then []
      else
        let seen = term :: seen in
        let check = check bound seen in
        match t_head term with
        | T_flex -> [ term ]
        | T_type -> []
        | T_var { var } ->
            if List.exists (v_same var) bound || Level.(v_at var <= t_at flex)
            then []
            else
              error (t_loc term)
                (Format.asprintf "unify: escape check for %a" Name.pp
                   (v_name var))
        | T_let _ -> error (t_loc term) "unify: unreduced let"
        | T_pi { param; body } -> check param @ check body
        | T_lambda { bound = var; body } ->
            check_lambda (var :: bound) seen body
        | T_apply { funct; arg } -> check funct @ check arg
        | T_self { body } | T_fix { body } -> check body
    and check_lambda bound seen body = check bound seen body in
    let lower = check [] [] term in
    List.iter (fun term -> t_lower term ~to_:(t_at flex)) lower

  let rec split fuel term spine =
    let term, fuel = reduce fuel [] term in
    match t_head term with
    | T_apply { funct; arg } -> split fuel funct (arg :: spine)
    | _ -> (term, spine, fuel)

  let unfold definitions term =
    let head, spine, _ = split reduction_limit term [] in
    let body =
      match t_head head with
      | T_var { var } ->
          List.find_opt (fun (bound, _) -> v_same bound var) definitions
          |> Option.map snd
      | T_fix { body } -> Some (t_apply (t_loc head) body head)
      | _ -> None
    in
    Option.map
      (fun body ->
        List.fold_left
          (fun funct arg -> t_apply (t_loc term) funct arg)
          body spine)
      body

  let rec unify_at ?(definitions = []) fuel received expected =
    let received, fuel = reduce fuel [] received in
    let expected, fuel = reduce fuel [] expected in
    if t_same received expected then fuel
    else
      let left, left_spine, fuel = split fuel received [] in
      let right, right_spine, fuel = split fuel expected [] in
      match (t_head left, t_head right) with
      | T_flex, T_flex when t_same left right ->
          unify_spine definitions fuel left_spine right_spine
      | T_flex, T_flex when left_spine = [] && right_spine = [] ->
          if Level.(t_at left <= t_at right) then unify_flex right [] left
          else unify_flex left [] right;
          fuel
      | T_flex, _ ->
          unify_flex left left_spine expected;
          fuel
      | _, T_flex ->
          unify_flex right right_spine received;
          fuel
      | _ -> unify_head definitions fuel received expected

  and unify_head definitions fuel received expected =
    let unify_at = unify_at ~definitions in
    match (t_head received, t_head expected) with
    | T_type, T_type -> fuel
    | T_var { var = left }, T_var { var = right } when v_same left right -> fuel
    | ( T_pi { param = left; body = left_body },
        T_pi { param = right; body = right_body } ) ->
        let fuel = unify_at fuel left right in
        unify_at fuel left_body right_body
    | ( T_lambda { bound = left; body = left_body },
        T_lambda { bound = right; body = right_body } ) ->
        let loc = t_loc received in
        let at = Level.succ (Level.max (v_at left) (v_at right)) in
        let var = v_fresh loc ~name:(v_name left) ~at () in
        unify_at fuel
          (subst left (t_var loc var) left_body)
          (subst right (t_var loc var) right_body)
    | T_lambda { bound; body }, _ ->
        unify_eta definitions fuel bound body expected
    | _, T_lambda { bound; body } ->
        unify_eta definitions fuel bound body received
    | T_self { body = left }, T_self { body = right }
    | T_fix { body = left }, T_fix { body = right } ->
        unify_at fuel left right
    | T_fix { body }, _ ->
        unify_at fuel (t_apply (t_loc received) body received) expected
    | _, T_fix { body } ->
        unify_at fuel received (t_apply (t_loc expected) body expected)
    | T_apply _, T_apply _ ->
        let left, left_spine, fuel = split fuel received [] in
        let right, right_spine, fuel = split fuel expected [] in
        let fuel = unify_at fuel left right in
        unify_spine definitions fuel left_spine right_spine
    | _ -> (
        match unfold definitions received with
        | Some received -> unify_at fuel received expected
        | None -> (
            match unfold definitions expected with
            | Some expected -> unify_at fuel received expected
            | None -> error (t_loc received) "unify: type clash"))

  and unify_spine definitions fuel received expected =
    match (received, expected) with
    | [], [] -> fuel
    | left :: received, right :: expected ->
        let fuel = unify_at ~definitions fuel left right in
        unify_spine definitions fuel received expected
    | term :: _, [] | [], term :: _ ->
        error (t_loc term) "unify: application arity clash"

  and unify_eta definitions fuel bound body other =
    let loc = t_loc other in
    let var =
      v_fresh loc ~name:(v_name bound)
        ~at:(Level.succ (Level.max (v_at bound) (t_at other)))
        ()
    in
    let arg = t_var loc var in
    unify_at ~definitions fuel (subst bound arg body) (t_apply loc other arg)

  and unify_flex flex spine to_ =
    let rec variables seen = function
      | [] -> []
      | arg :: spine -> (
          match t_head (whnf arg) with
          | T_var { var } ->
              if List.exists (v_same var) seen then
                error (t_loc arg) "unify: duplicated variable";
              var :: variables (var :: seen) spine
          | _ -> error (t_loc arg) "unify: not a pattern")
    in
    let vars = variables [] spine in
    let to_ =
      List.fold_right (fun var body -> t_lambda (v_loc var) var body) vars to_
    in
    let () = unify_check ~flex to_ in
    t_link flex ~to_

  let unify ?(definitions = []) received expected =
    ignore (unify_at ~definitions reduction_limit received expected)
end

module Elaborate = struct
  open Typed_tree
  module S = Syntax_tree

  type binding = { name : Name.t; var : var; received : term }

  type context =
    | Context of {
        vars : binding list;
        at : Level.t;
        (* Scoped equations for checking an explicit fix body. Recursive names
           stay rigid in metavariable spines and unfold only for conversion. *)
        definitions : (var * term) list;
      }

  let initial_context loc =
    let type_ = t_type loc in
    let name = Name.make "Type" in
    let var = v_fresh loc ~to_:type_ ~name ~at:Level.zero () in
    Context
      {
        vars = [ { name; var; received = type_ } ];
        at = Level.zero;
        definitions = [];
      }

  let level (Context { at; _ }) = at

  let lookup (Context { vars; _ }) name =
    List.find_opt (fun binding -> Name.equal binding.name name) vars

  let copy_context (Context { vars; at; definitions }) =
    let vars, state =
      List.fold_right
        (fun { name; var; received } (vars, state) ->
          let var, state = copy_var state var in
          let received, state = copy_term state received in
          ({ name; var; received } :: vars, state))
        vars
        ([], ([], []))
    in
    let definitions, _ =
      List.fold_right
        (fun (var, body) (definitions, state) ->
          let var, state = copy_var state var in
          let body, state = copy_term state body in
          ((var, body) :: definitions, state))
        definitions ([], state)
    in
    Context { vars; at; definitions }

  let with_let loc (Context { vars; at; definitions }) name received value =
    let at = Level.succ at in
    let var = v_fresh loc ~to_:value ~name ~at () in
    (Context { vars = { name; var; received } :: vars; at; definitions }, var)

  let t_flex loc (Context { vars; _ }) () =
    let vars =
      List.rev_map (fun binding -> binding.var) vars
      |> List.filter (fun (Var { to_; _ }) -> Option.is_none to_)
    in
    let flex = Typed_tree.t_flex loc ~at:Level.zero () in
    List.fold_left
      (fun funct var -> t_apply loc funct (t_var loc var))
      flex vars

  let t_flex_pi loc ctx () =
    let param = t_flex loc ctx () in
    let body = t_flex loc ctx () in
    (t_pi loc param body, param, body)

  let t_flex_self loc ctx () =
    let body = t_flex loc ctx () in
    (t_self loc body, body)

  let with_def loc (Context { vars; at; definitions }) name body =
    let at = Level.succ at in
    let var = v_fresh loc ~name ~at () in
    let received = t_apply loc body (t_var loc var) in
    (Context { vars = { name; var; received } :: vars; at; definitions }, var)

  let match_pi loc ctx type_ =
    match t_head (whnf type_) with
    | T_pi { param; body } -> (param, body)
    | _ ->
        let pi, param, body = t_flex_pi loc ctx () in
        let () = unify type_ pi in
        (param, body)

  type pat = P_var of { var : var } | P_apply of { funct : pat; arg : var }

  let rec p_var = function
    | P_var { var } -> var
    | P_apply { funct; _ } -> p_var funct

  let rec expand_pat loc pat body =
    match pat with
    | P_var { var } -> (var, body)
    | P_apply { funct; arg } -> expand_pat loc funct (t_lambda loc arg body)

  let specialize loc ctx body value =
    let head, _, _ = split reduction_limit body [] in
    let () =
      match (t_head head, t_head (whnf value)) with
      | T_flex, T_var _ -> ()
      | T_flex, _ ->
          let var =
            v_fresh loc ~name:(Name.make "x") ~at:(Level.succ (level ctx)) ()
          in
          unify body (t_lambda loc var (t_flex loc ctx ()))
      | _ -> ()
    in
    apply loc body value

  let rec check_type loc ctx fuel value received expected =
    if fuel = 0 then error loc "recursive self type does not expose a type";
    let received = whnf received and expected = whnf expected in
    if t_same received expected then ()
    else
      match (t_head received, t_head expected) with
      | T_self { body = received }, T_self { body = expected } ->
          check_type loc ctx (fuel - 1) value
            (specialize loc ctx received value)
            (specialize loc ctx expected value)
      | T_self { body }, (T_pi _ | T_type) ->
          check_type loc ctx (fuel - 1) value
            (specialize loc ctx body value)
            expected
      | _, T_self { body } ->
          check_type loc ctx (fuel - 1) value received
            (specialize loc ctx body value)
      | _ -> (
          let (Context { definitions; _ }) = ctx in
          let flexible term =
            let head, _, _ = split reduction_limit term [] in
            match t_head head with T_flex -> true | _ -> false
          in
          if flexible received || flexible expected then unify received expected
          else
            match unfold definitions received with
            | Some received ->
                check_type loc ctx (fuel - 1) value received expected
            | None -> (
                match unfold definitions expected with
                | Some expected ->
                    check_type loc ctx (fuel - 1) value received expected
                | None -> unify ~definitions received expected))

  let finish loc ctx value received expected self =
    let () = check_type loc ctx 100 value received expected in
    unify value self

  let rec check_term ctx (S.Term { head; loc }) expected self =
    match head with
    | S.T_var { var = name } -> (
        match lookup ctx name with
        | Some { var; received; _ } ->
            let value = t_var loc var in
            finish loc ctx value received expected self
        | None ->
            error loc (Format.asprintf "unbound variable: %a" Name.pp name))
    | S.T_apply { funct; arg } ->
        let funct_type, arg_type, body_type = t_flex_pi loc ctx () in
        let funct_self = t_flex loc ctx () in
        let arg_self = t_flex loc ctx () in
        let () = check_term ctx funct funct_type funct_self in
        let () = check_term ctx arg arg_type arg_self in
        let received = apply loc body_type arg_self in
        let value = t_apply loc funct_self arg_self in
        finish loc ctx value received expected self
    | S.T_self { bound } ->
        (* Expose the self-type shape before checking dependent annotations;
           an enclosing recursive equation may already need to unfold it. *)
        let _, _, _, outer_type = infer_pat ~self ctx bound in
        finish loc ctx outer_type (t_type loc) expected self
    | S.T_fix { bound; body } ->
        let inner_ctx, bound, inner_type, outer_type = infer_pat ctx bound in
        let body_self = t_flex loc inner_ctx () in
        let var, expanded = expand_pat loc bound body_self in
        let (Context { vars; at; definitions }) = inner_ctx in
        let inner_ctx =
          Context { vars; at; definitions = (var, expanded) :: definitions }
        in
        let () = check_term inner_ctx body inner_type body_self in
        let value = t_fix loc (t_lambda loc var expanded) in
        finish loc ctx value outer_type expected self
    | S.T_block { block; return } -> check_block ctx block return expected self
    | S.T_sigma _ -> error loc "check_term: sigma types are not implemented"
    | S.T_unit -> error loc "check_term: unit is not implemented"
    | S.T_pair _ -> error loc "check_term: pairs are not implemented"
    | S.T_box _ -> error loc "check_term: boxes are not implemented"

  and check_block ctx (S.Block { head; loc }) return expected self =
    match head with
    | S.B_hole -> check_term ctx return expected self
    | S.B_def _ -> error loc "check_block: declarations are not implemented"
    | S.B_fix { bound; arg; body } ->
        let name, received, value = infer_let ctx bound arg in
        let (S.Pat { loc = bound_loc; _ }) = bound in
        let ctx, var = with_let bound_loc ctx name received value in
        let body_value = t_flex loc ctx () in
        let () = check_block ctx body return expected body_value in
        unify (t_let loc var value body_value) self

  and infer_let ctx bound arg =
    let rec split (S.Pat { head; loc }) parts =
      match head with
      | S.P_var { var } -> (var, parts)
      | S.P_apply { funct; arg } -> split funct (`Param arg :: parts)
      | S.P_annot { pat; annot } -> split pat (`Annot annot :: parts)
      | S.P_unit -> error loc "infer_let: unit is not implemented"
      | S.P_pair _ -> error loc "infer_let: pairs are not implemented"
      | S.P_box _ -> error loc "infer_let: boxes are not implemented"
    in
    let name, parts = split bound [] in
    let (S.Pat { loc; _ }) = bound in
    let received = t_flex loc ctx () in
    let rec elaborate ctx expected = function
      | [] ->
          let value = t_flex loc ctx () in
          let () = check_term ctx arg expected value in
          value
      | `Annot (S.Term { loc; _ } as annot) :: parts ->
          let () = check_term ctx annot (t_type loc) expected in
          elaborate ctx expected parts
      | `Param (S.Pat { loc; _ } as param) :: parts ->
          let param_type, body_type = match_pi loc ctx expected in
          let ctx, param, _ = check_pat ctx param param_type in
          let var = p_var param in
          let expected = t_apply loc body_type (t_var loc var) in
          t_lambda loc var (elaborate ctx expected parts)
    in
    (* Only parameters are in scope on the right-hand side. The let name is
       added by check_block after its value and annotations have been checked. *)
    (name, received, elaborate ctx received parts)

  and check_pat ctx pat expected =
    (* An applied parameter binds its root here. Its own arguments belong only
       to its type, where infer_pat has already abstracted them. *)
    let (Context { vars; definitions; _ }) = ctx in
    let Context { vars = inner_vars; _ }, pat, inner_type, outer_type =
      infer_pat ctx pat
    in
    let () = unify outer_type expected in
    let var = p_var pat in
    let binding =
      List.find (fun binding -> v_same binding.var var) inner_vars
    in
    let ctx = Context { vars = binding :: vars; at = v_at var; definitions } in
    (ctx, pat, inner_type)

  and infer_pat ?self ctx (S.Pat { head; loc }) =
    match head with
    | S.P_annot { pat; annot } ->
        let ctx, pat, inner_type, outer_type = infer_pat ?self ctx pat in
        let () = check_term ctx annot (t_type loc) inner_type in
        (ctx, pat, inner_type, outer_type)
    | S.P_var { var = name } ->
        let outer_type, body_type = t_flex_self loc ctx () in
        let () = Option.iter (unify outer_type) self in
        let ctx, var = with_def loc ctx name body_type in
        let inner_type = t_apply loc body_type (t_var loc var) in
        (ctx, P_var { var }, inner_type, outer_type)
    | S.P_apply { funct; arg } ->
        let ctx, funct, inner_type, outer_type = infer_pat ?self ctx funct in
        let param_type, body_type = match_pi loc ctx inner_type in
        let ctx, arg, _ = check_pat ctx arg param_type in
        let arg = p_var arg in
        let inner_type = t_apply loc body_type (t_var loc arg) in
        (ctx, P_apply { funct; arg }, inner_type, outer_type)
    | S.P_unit -> error loc "infer_pat: unit is not implemented"
    | S.P_pair _ -> error loc "infer_pat: pairs are not implemented"
    | S.P_box _ -> error loc "infer_pat: boxes are not implemented"

  let infer_in ctx (S.Term { loc; _ } as term) =
    let expected = t_flex loc ctx () in
    let self = t_flex loc ctx () in
    let () = check_term ctx term expected self in
    (expected, self)

  let infer (S.Term { loc; _ } as term) = infer_in (initial_context loc) term
end
