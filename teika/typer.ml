open Utils
open Syntax

module Substs : sig
  open Ttree

  type substs
  type t = substs

  val size : substs -> Level.t
  val empty : substs
  val push : to_:term -> at_:Level.t -> substs -> substs
  val find_local : var:Index.t -> substs -> term * Level.t
end = struct
  open Ttree

  (* TODO: especialized patricia trie *)
  type substs =
    | Substs of { next : Level.t; content : (term * Level.t) Level.Map.t }

  type t = substs

  let size substs =
    let (Substs { next; content = _ }) = substs in
    next

  let empty = Substs { next = Level.zero; content = Level.Map.empty }

  (* TODO: better name than at_ *)
  let push ~to_ ~at_ substs =
    let (Substs { next; content }) = substs in
    let content = Level.Map.add next (to_, at_) content in
    let next = Level.next next in
    Substs { next; content }

  let find_local ~var substs =
    let (Substs { next; content }) = substs in
    (* TODO: proper errors here *)
    let level = Option.get @@ Level.level_of_index ~next ~var in
    Level.Map.find level content
end

module Machinery = struct
  open Ttree
  open Terror
  (* TODO: cache normalization? *)

  let tt_split_term term =
    match term with
    | TT_typed { term; type_ } -> (term, type_)
    (* sort *)
    | TT_free_var { var } when Level.equal var level_univ -> (term, term)
    | _ -> error_invariant_term_untyped term

  let tp_type_of pat =
    match pat with
    | TP_typed { pat = _; type_ } -> type_
    | _ -> error_invariant_pat_untyped pat

  (* TODO: reduce allocations? Maybe cata over term? *)
  (* TODO: tag terms without bound variables, aka no shifting *)
  (* TODO: tag terms without binders *)
  (* TODO: tag terms locally closed *)
  (* TODO: gas count *)
  let rec tt_shift ~by_ ~depth term =
    let tt_shift ~depth term = tt_shift ~by_ ~depth term in
    let tp_shift ~depth pat = tp_shift ~by_ ~depth pat in
    match term with
    | TT_typed { term; type_ } ->
        let type_ = tt_shift ~depth type_ in
        tt_typed ~type_ @@ tt_shift ~depth term
    | TT_annot { term; annot } ->
        let annot = tt_shift ~depth annot in
        let term = tt_shift ~depth term in
        tt_annot ~term ~annot
    | TT_free_var { var = _ } as term -> term
    | TT_bound_var { var } as term -> (
        match Index.(var < depth) with
        | true -> term
        | false ->
            let var = Index.shift var ~by_ in
            tt_bound_var ~var)
    | TT_forall { param; return } ->
        let param = tp_shift ~depth param in
        let return =
          let depth = Index.next depth in
          tt_shift ~depth return
        in
        tt_forall ~param ~return
    | TT_lambda { param; return } ->
        let param = tp_shift ~depth param in
        let return =
          let depth = Index.next depth in
          tt_shift ~depth return
        in
        tt_lambda ~param ~return
    | TT_apply { lambda; arg } ->
        let lambda = tt_shift ~depth lambda in
        let arg = tt_shift ~depth arg in
        tt_apply ~lambda ~arg
    | TT_let { bound; value; return } ->
        let bound = tp_shift ~depth bound in
        let value = tt_shift ~depth value in
        let return =
          let depth = Index.next depth in
          tt_shift ~depth return
        in
        tt_let ~bound ~value ~return
    | TT_string { literal = _ } as term -> term

  and tp_shift ~by_ ~depth pat =
    let tt_shift ~depth term = tt_shift ~by_ ~depth term in
    let tp_shift ~depth pat = tp_shift ~by_ ~depth pat in
    match pat with
    | TP_typed { pat; type_ } ->
        let type_ = tt_shift ~depth type_ in
        tp_typed ~type_ @@ tp_shift ~depth pat
    | TP_annot { pat; annot } ->
        let annot = tt_shift ~depth annot in
        let pat = tp_shift ~depth pat in
        tp_annot ~annot ~pat
    | TP_var _ as pat -> pat

  let tt_shift ~by_ term = tt_shift ~by_ ~depth:Index.zero term

  let rec tt_expand_head term ~substs =
    match term with
    | TT_typed { term; type_ = _ } ->
        (* typed *)
        tt_expand_head term ~substs
    | TT_annot { term; annot = _ } ->
        (* annot *)
        tt_expand_head term ~substs
    | TT_bound_var { var } ->
        (* subst *)
        let term =
          let term, at_ = Substs.find_local ~var substs in
          let by_ = (Level.repr @@ Substs.size substs) - Level.repr at_ in
          (* TODO: not very fast *)
          tt_shift ~by_ term
        in
        tt_expand_head term ~substs
    | TT_apply { lambda; arg } -> (
        match tt_expand_head lambda ~substs with
        | TT_lambda { param = _; return }, lambda_substs ->
            (* beta *)
            let substs =
              let at_ = Substs.size substs in
              Substs.push ~to_:arg ~at_ lambda_substs
            in
            tt_expand_head return ~substs
        | lambda, lambda_substs ->
            (* head *)
            let arg =
              let by_ =
                (Level.repr @@ Substs.size lambda_substs)
                - (Level.repr @@ Substs.size substs)
              in
              tt_shift ~by_ arg
            in
            (tt_apply ~lambda ~arg, lambda_substs))
    | TT_let { bound = _; value; return = term } ->
        (* zeta *)
        let substs =
          let at_ = Substs.size substs in
          Substs.push ~to_:value ~at_ substs
        in
        tt_expand_head term ~substs
    | TT_free_var _ | TT_forall _ | TT_lambda _ | TT_string _ ->
        (* head *)
        (term, substs)

  (* TODO: avoid cyclical expansion *)
  (* TODO: avoid allocating term nodes *)
  (* TODO: short circuit when same variable on both sides *)
  let rec tt_equal ~level ~left ~left_substs ~right ~right_substs =
    let left, left_substs = tt_expand_head left ~substs:left_substs in
    let right, right_substs = tt_expand_head right ~substs:right_substs in
    tt_struct_equal ~level ~left ~left_substs ~right ~right_substs

  and tt_struct_equal ~level ~left ~left_substs ~right ~right_substs =
    match (left, right) with
    | TT_typed _, _ | _, TT_typed _ -> failwith "invariant"
    | TT_annot _, _ | _, TT_annot _ -> failwith "invariant"
    | TT_let _, _ | _, TT_let _ -> failwith "invariant"
    | TT_bound_var _, _ | _, TT_bound_var _ -> failwith "invariant"
    | TT_free_var { var = left }, TT_free_var { var = right }
      when Level.equal left right ->
        ()
    | ( TT_forall { param = left_param; return = left_return },
        TT_forall { param = right_param; return = right_return } ) ->
        with_tp_equal_contra ~level ~left:left_param ~left_substs
          ~right:right_param ~right_substs
        @@ fun ~level ~left_substs ~right_substs ->
        tt_equal ~level ~left:left_return ~left_substs ~right:right_return
          ~right_substs
    | ( TT_lambda { param = left_param; return = left_return },
        TT_lambda { param = right_param; return = right_return } ) ->
        with_tp_equal_contra ~level ~left:left_param ~left_substs
          ~right:right_param ~right_substs
        @@ fun ~level ~left_substs ~right_substs ->
        tt_equal ~level ~left:left_return ~left_substs ~right:right_return
          ~right_substs
    | ( TT_apply { lambda = left_lambda; arg = left_arg },
        TT_apply { lambda = right_lambda; arg = right_arg } ) ->
        tt_equal ~level ~left:left_lambda ~left_substs ~right:right_lambda
          ~right_substs;
        tt_equal ~level ~left:left_arg ~left_substs ~right:right_arg
          ~right_substs
    | TT_string { literal = left }, TT_string { literal = right }
      when String.equal left right ->
        ()
    | ( (TT_free_var _ | TT_forall _ | TT_lambda _ | TT_apply _ | TT_string _),
        (TT_free_var _ | TT_forall _ | TT_lambda _ | TT_apply _ | TT_string _) )
      ->
        (* TODO: type clash needs substs *)
        error_type_clash ~left ~right

  and with_tp_equal_contra ~level ~left ~left_substs ~right ~right_substs k =
    (* TODO: contra? *)
    let left_type = tp_type_of left in
    let right_type = tp_type_of right in
    tt_equal ~level ~left:left_type ~left_substs ~right:right_type ~right_substs;
    let level = Level.next level in
    let left_to_ =
      (* TODO: maybe pack left_type? *)
      tt_typed ~type_:left_type @@ tt_free_var ~var:level
    in
    let right_to_ =
      (* TODO: maybe pack right_type? *)
      tt_typed ~type_:right_type @@ tt_free_var ~var:level
    in
    let left_substs =
      let at_ = Substs.size left_substs in
      Substs.push ~to_:left_to_ ~at_ left_substs
    in
    let right_substs =
      let at_ = Substs.size right_substs in
      Substs.push ~to_:right_to_ ~at_ right_substs
    in
    k ~level ~left_substs ~right_substs

  (* TODO: linear functions can be faster without packing *)
  (* TODO: variable expansions without args can be faster without packing *)

  (* TODO: this is super hackish *)
  let rec tt_split_forall type_ ~args ~substs =
    match (type_, args) with
    | TT_forall { param; return }, [] ->
        let wrapped_arg_type = tp_type_of param in
        let apply_return_type ~arg ~at_ =
          let arg =
            let by_ = (Level.repr @@ Substs.size substs) - Level.repr at_ in
            tt_shift ~by_ arg
          in
          tt_typed ~type_:tt_type_univ @@ tt_let ~bound:param ~value:arg ~return
        in
        (wrapped_arg_type, apply_return_type)
    (* annot *)
    | TT_typed { term = type_; type_ = _ }, _ ->
        tt_split_forall type_ ~args ~substs
    | TT_annot { term = type_; annot = _ }, _ ->
        tt_split_forall type_ ~args ~substs
    (* substs *)
    | TT_bound_var { var }, _ ->
        let type_ =
          let type_, at_ = Substs.find_local ~var substs in
          let by_ = (Level.repr @@ Substs.size substs) - Level.repr at_ in
          (* TODO: not very fast *)
          tt_shift ~by_ type_
        in
        tt_split_forall type_ ~args ~substs
    (* beta *)
    | TT_lambda { param; return = type_ }, (arg, at_) :: args ->
        tt_split_forall_subst ~bound:param ~value:arg ~at_ type_ ~args ~substs
    | TT_apply { lambda; arg }, args ->
        let args = (arg, Substs.size substs) :: args in
        tt_split_forall lambda ~args ~substs
    (* let *)
    | TT_let { bound; value; return = type_ }, _ ->
        let at_ = Substs.size substs in
        tt_split_forall_subst ~bound ~value ~at_ type_ ~args ~substs
    (* head *)
    | TT_free_var _, _ | TT_forall _, _ | TT_lambda _, [] | TT_string _, _ ->
        (* TODO: dump substs *)
        error_not_a_forall ~type_

  and tt_split_forall_subst ~bound ~value ~at_ type_ ~args ~substs =
    let substs = Substs.push ~to_:value ~at_ substs in
    let wrapped_arg_type, apply_return_type =
      tt_split_forall type_ ~args ~substs
    in
    let wrapped_arg_type =
      tt_typed ~type_:tt_type_univ
      @@ tt_let ~bound ~value ~return:wrapped_arg_type
    in
    let apply_return_type ~arg ~at_ =
      tt_typed ~type_:tt_type_univ
      @@ tt_let ~bound ~value ~return:(apply_return_type ~arg ~at_)
    in
    (wrapped_arg_type, apply_return_type)
end

module Elaborate_context : sig
  type context

  val initial : context

  (* vars *)
  val lookup_var : context -> Name.t -> Index.t
  val with_bound_var : context -> Name.t -> (context -> 'k) -> 'k
end = struct
  open Ttree
  open Terror

  type context = { level : Level.t; names : Level.t Name.Map.t }

  let initial =
    (* TODO: this is bad *)
    let names =
      let open Name.Map in
      let names = empty in
      (* TODO: duplicated string name *)
      let names = add (Name.make "Type") level_univ names in
      let names = add (Name.make "String") level_string names in
      names
    in
    let level = level_string in
    { names; level }

  let lookup_var ctx name =
    let { names; level } = ctx in
    match Name.Map.find_opt name names with
    | Some from -> (
        match Level.offset ~from ~to_:level with
        | Some var -> var
        | None -> failwith "invariant lookup")
    | None -> error_unknown_var ~name

  let with_bound_var ctx name k =
    let { level; names } = ctx in
    let level = Level.next level in
    let names = Name.Map.add name level names in
    k @@ { level; names }
end

module Elaborate = struct
  open Ltree
  open Ttree
  open Terror
  open Elaborate_context

  (* TODO: does having expected_term also improves inference?
       Maybe with self and fix? But maybe not worth it
     Seems to help with many cases such as expected on annotation *)

  let rec elaborate_term ctx term =
    (* TODO: use this location *)
    let (LTerm { term; loc = _ }) = term in
    let tt_typed term =
      (* TODO: some of those could already be typed *)
      tt_typed ~type_:tt_nil term
    in

    match term with
    | LT_annot { term; annot } ->
        let annot = elaborate_term ctx annot in
        let term = elaborate_term ctx term in
        tt_typed @@ tt_annot ~term ~annot
    | LT_var { var = name } ->
        let var = lookup_var ctx name in
        tt_typed @@ tt_bound_var ~var
    | LT_extension _ -> error_extensions_not_implemented ()
    | LT_forall { param; return } ->
        with_elaborate_pat ctx param @@ fun ctx param ->
        let return = elaborate_term ctx return in
        tt_typed @@ tt_forall ~param ~return
    | LT_lambda { param; return } ->
        with_elaborate_pat ctx param @@ fun ctx param ->
        let return = elaborate_term ctx return in
        tt_typed @@ tt_lambda ~param ~return
    | LT_apply { lambda; arg } ->
        let lambda = elaborate_term ctx lambda in
        let arg = elaborate_term ctx arg in
        tt_typed @@ tt_apply ~lambda ~arg
    | LT_hoist _ -> error_hoist_not_implemented ()
    | LT_let { bound; value; return } ->
        let value = elaborate_term ctx value in
        (* TODO: this should be before value *)
        with_elaborate_pat ctx bound @@ fun ctx bound ->
        let return = elaborate_term ctx return in
        tt_typed @@ tt_let ~bound ~value ~return
    | LT_string { literal } -> tt_typed @@ tt_string ~literal

  and with_elaborate_pat ctx pat k =
    (* TODO: to_ here *)
    (* TODO: use this location *)
    let (LPat { pat; loc = _ }) = pat in
    let tp_typed pat =
      (* TODO: some of those could already be typed *)
      tp_typed ~type_:tt_nil pat
    in
    match pat with
    | LP_var { var = name } ->
        with_bound_var ctx name @@ fun ctx -> k ctx @@ tp_typed @@ tp_var ~name
    | LP_annot { pat; annot } ->
        let annot = elaborate_term ctx annot in
        with_elaborate_pat ctx pat @@ fun ctx pat ->
        k ctx @@ tp_typed @@ tp_annot ~pat ~annot
end

module Infer_context : sig
  open Ttree

  type context

  val initial : context

  (* vars *)
  val find_var_type : context -> Index.t -> term
  val with_bound_var : context -> type_:term -> (context -> 'k) -> 'k
  val with_subst_var : context -> to_:term -> (context -> 'k) -> 'k

  (* machinery *)
  val tt_equal : context -> left:term -> right:term -> unit
  val tt_split_forall : context -> term -> term * (arg:term -> term)
end = struct
  open Ttree

  type context = {
    level : Level.t;
    left_substs : Substs.t;
    right_substs : Substs.t;
  }

  let initial =
    let level = level_string in
    let substs =
      let substs = Substs.empty in
      let substs =
        let at_ = Substs.size substs in
        Substs.push ~to_:tt_nil ~at_ substs
      in
      let substs =
        let at_ = Substs.size substs in
        Substs.push ~to_:tt_type_univ ~at_ substs
      in
      let substs =
        let at_ = Substs.size substs in
        Substs.push ~to_:tt_type_string ~at_ substs
      in
      substs
    in
    { level; left_substs = substs; right_substs = substs }

  let find_var_type ctx var =
    let { level = _; left_substs; right_substs = _ } = ctx in
    let term, at_ = Substs.find_local ~var left_substs in
    let _term, type_ = Machinery.tt_split_term term in
    let by_ = (Level.repr @@ Substs.size left_substs) - Level.repr at_ in
    Machinery.tt_shift ~by_ type_

  let with_bound_var ctx ~type_ k =
    let { level; left_substs; right_substs } = ctx in
    let level = Level.next level in
    let to_ = tt_typed ~type_ @@ tt_free_var ~var:level in
    let left_substs =
      let at_ = Substs.size left_substs in
      Substs.push ~to_ ~at_ left_substs
    in
    let right_substs =
      let at_ = Substs.size right_substs in
      Substs.push ~to_ ~at_ right_substs
    in
    k @@ { level; left_substs; right_substs }

  let with_subst_var ctx ~to_ k =
    let { level; left_substs; right_substs } = ctx in
    let level = Level.next level in
    let left_substs =
      let at_ = Substs.size left_substs in
      Substs.push ~to_ ~at_ left_substs
    in
    let right_substs =
      let at_ = Substs.size right_substs in
      Substs.push ~to_ ~at_ right_substs
    in
    k @@ { level; left_substs; right_substs }

  let tt_split_forall ctx type_ =
    let { level = _; left_substs; right_substs = _ } = ctx in
    (* TODO: left and right? *)
    let wrapped_arg_type, apply_return_type =
      Machinery.tt_split_forall type_ ~args:[] ~substs:left_substs
    in
    let apply_return_type ~arg =
      let at_ = Substs.size left_substs in
      apply_return_type ~arg ~at_
    in
    (wrapped_arg_type, apply_return_type)

  let tt_equal ctx ~left ~right =
    let { level; left_substs; right_substs } = ctx in
    Format.eprintf "left : %a, right : %a\n%!" Tprinter.pp_term left
      Tprinter.pp_term right;
    Machinery.tt_equal ~level ~left ~left_substs ~right ~right_substs
end

module Infer = struct
  open Ttree
  open Terror
  open Infer_context

  (* TODO: does having expected_term also improves inference?
       Maybe with self and fix? But maybe not worth it
     Seems to help with many cases such as expected on annotation *)

  let rec infer_term ctx term =
    match term with
    | TT_free_var { var = _ } -> failwith "unreachable"
    | TT_typed ({ term; type_ = _ } as typed) ->
        let type_ = infer_term ctx term in
        typed.type_ <- type_;
        type_
    | TT_annot { term; annot } ->
        (* TODO: expected term could propagate here *)
        let () = check_annot ctx annot in
        (* TODO: unify annot before or after check term *)
        let () = check_term ctx term ~expected:annot in
        tt_typed ~type_:tt_type_univ @@ annot
    | TT_bound_var { var } -> find_var_type ctx var
    | TT_forall { param; return } ->
        with_infer_pat ctx param @@ fun ctx ->
        let () = check_annot ctx return in
        tt_typed ~type_:tt_type_univ @@ tt_type_univ
    | TT_lambda { param; return } ->
        with_infer_pat ctx param @@ fun ctx ->
        let return = infer_term ctx return in
        tt_typed ~type_:tt_type_univ @@ tt_forall ~param ~return
    | TT_apply { lambda; arg } ->
        let forall = infer_term ctx lambda in
        (* TODO: this could be better? avoiding split forall? *)
        let wrapped_arg_type, apply_return_type = tt_split_forall ctx forall in
        let () = check_term ctx arg ~expected:wrapped_arg_type in
        tt_typed ~type_:tt_type_univ @@ apply_return_type ~arg
    | TT_let { bound; value; return } ->
        (* TODO: use this loc *)
        let value_type = infer_term ctx value in
        (* TODO: this should be before value *)
        (* TODO: with_check_pat + subst  *)
        with_check_pat ctx bound ~expected:value_type ~to_:value @@ fun ctx ->
        let return_type = infer_term ctx return in
        tt_typed ~type_:tt_type_univ @@ tt_let ~bound ~value ~return:return_type
    | TT_string { literal = _ } ->
        tt_typed ~type_:tt_type_univ @@ tt_type_string

  and check_term ctx term ~expected =
    (* TODO: let () = assert_is_tt_with_type expected in *)
    (* TODO: expected should be a pattern? *)
    (* TODO: propagation through dependent things
        aka substitution inversion *)
    (* TODO: forall could in theory be improved by expected term *)
    (* TODO: apply could propagate when arg is var *)
    (* TODO: could propagate through let? *)
    let received = infer_term ctx term in
    tt_equal ctx ~left:received ~right:expected

  and check_annot ctx term = check_term ctx term ~expected:tt_type_univ

  and with_infer_pat ctx pat k =
    let type_ = infer_pat ctx pat in
    with_bound_var ctx ~type_ k

  and infer_pat ctx pat =
    match pat with
    | TP_typed ({ pat; type_ = _ } as typed) ->
        let type_ = infer_pat ctx pat in
        typed.type_ <- type_;
        type_
    | TP_var { name = _ } -> error_missing_annotation ()
    | TP_annot { pat; annot } ->
        let () = check_annot ctx annot in
        let () = check_pat ctx pat ~expected:annot in
        annot

  and with_check_pat ctx pat ~expected ~to_ k =
    let () = check_pat ctx pat ~expected in
    with_subst_var ctx ~to_ k

  and check_pat ctx pat ~expected =
    (* TODO: let () = assert_is_tt_with_type expected in *)
    (* TODO: expected should be a pattern, to achieve strictness *)
    match pat with
    | TP_typed ({ pat; type_ = _ } as typed) ->
        typed.type_ <- expected;
        check_pat ctx pat ~expected
    | TP_annot { pat; annot } ->
        let () = check_annot ctx annot in
        let () = tt_equal ctx ~left:annot ~right:expected in
        check_pat ctx pat ~expected:annot
    | TP_var { name = _ } -> ()

  let infer_term term =
    match
      let term = Elaborate.elaborate_term Elaborate_context.initial term in
      infer_term Infer_context.initial term
    with
    | term -> Ok term
    | exception TError { error } -> Error error
end
