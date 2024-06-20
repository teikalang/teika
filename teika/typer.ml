open Utils
open Syntax

module Substs : sig
  open Ttree

  type substs
  type t = substs

  val size : substs -> Level.t
  val empty : substs
  val push : to_:term -> at_:substs -> substs -> substs
  val find_global : var:Level.t -> substs -> term * substs
  val find_local : var:Index.t -> substs -> term * substs
end = struct
  open Ttree

  (* TODO: especialized patricia trie *)
  type substs =
    | Substs of { next : Level.t; content : (term * substs) Level.Map.t }

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

  let find_global ~var substs =
    let (Substs { next = _; content }) = substs in
    (* TODO: proper errors here *)
    Level.Map.find var content

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

  let tt_type_of term =
    match term with
    (* sort *)
    | TT_rigid_var { var } when Level.equal var level_univ -> term
    (* wrappers *)
    | TT_typed { term = _; type_ } -> type_
    | TT_annot { term = _; annot } -> annot
    | _ -> error_invariant_term_untyped term

  let tp_type_of pat =
    match pat with
    | TP_typed { pat = _; type_ } -> type_
    | TP_annot { pat = _; annot } -> annot
    | _ -> error_invariant_pat_untyped pat

  (* TODO: reduce allocations? Maybe cata over term? *)
  (* TODO: tag terms without bound variables, aka no shifting *)
  (* TODO: tag terms without binders *)
  (* TODO: tag terms locally closed *)
  (* TODO: gas count *)
  let rec tt_pack ~size ~depth term =
    let tt_pack ~depth term = tt_pack ~size ~depth term in
    let tp_pack ~depth pat = tp_pack ~size ~depth pat in
    match term with
    | TT_typed { term; type_ } ->
        let type_ = tt_pack ~depth type_ in
        tt_typed ~type_ @@ tt_pack ~depth term
    | TT_annot { term; annot } ->
        let annot = tt_pack ~depth annot in
        let term = tt_pack ~depth term in
        tt_annot ~term ~annot
    | TT_rigid_var { var = _ } as term -> term
    | TT_global_var { var } as term -> (
        match Level.(var < size) with
        | true -> term
        | false -> (
            match Level.global_to_local ~size ~var ~depth with
            | Some var -> tt_local_var ~var
            | None -> term))
    | TT_local_var { var } as term -> (
        match Index.(var < depth) with
        | true -> term
        | false -> (
            match Level.local_to_global ~size ~var ~depth with
            | Some var -> tt_global_var ~var
            | None -> term))
    | TT_forall { param; return } ->
        let param = tp_pack ~depth param in
        let return =
          let depth = Index.next depth in
          tt_pack ~depth return
        in
        tt_forall ~param ~return
    | TT_lambda { param; return } ->
        let param = tp_pack ~depth param in
        let return =
          let depth = Index.next depth in
          tt_pack ~depth return
        in
        tt_lambda ~param ~return
    | TT_apply { lambda; arg } ->
        let lambda = tt_pack ~depth lambda in
        let arg = tt_pack ~depth arg in
        tt_apply ~lambda ~arg
    | TT_let { bound; value; return } ->
        let bound = tp_pack ~depth bound in
        let value = tt_pack ~depth value in
        let return =
          let depth = Index.next depth in
          tt_pack ~depth return
        in
        tt_let ~bound ~value ~return
    | TT_string { literal = _ } as term -> term

  and tp_pack ~size ~depth pat =
    let tt_pack ~depth term = tt_pack ~size ~depth term in
    let tp_pack ~depth pat = tp_pack ~size ~depth pat in
    match pat with
    | TP_typed { pat; type_ } ->
        let type_ = tt_pack ~depth type_ in
        tp_typed ~type_ @@ tp_pack ~depth pat
    | TP_annot { pat; annot } ->
        let annot = tt_pack ~depth annot in
        let pat = tp_pack ~depth pat in
        tp_annot ~annot ~pat
    | TP_var _ as pat -> pat

  let tt_pack ~size term = tt_pack ~size ~depth:Index.zero term

  let rec with_tt_expand_head term ~args ~substs k =
    match (term, args) with
    (* annot *)
    | TT_typed { term; type_ = _ }, _ ->
        with_tt_expand_head term ~args ~substs k
    | TT_annot { term; annot = _ }, _ ->
        with_tt_expand_head term ~args ~substs k
    (* substs *)
    | TT_global_var { var }, _ ->
        let term, substs = Substs.find_global ~var substs in
        with_tt_expand_head term ~args ~substs k
    | TT_local_var { var }, _ ->
        let term, substs = Substs.find_local ~var substs in
        with_tt_expand_head term ~args ~substs k
    (* beta *)
    | TT_lambda { param = _; return = term }, (arg, arg_substs) :: args ->
        let substs = Substs.push ~to_:arg ~at_:arg_substs substs in
        with_tt_expand_head term ~args ~substs k
    | TT_apply { lambda = term; arg }, _ ->
        let args = (arg, substs) :: args in
        with_tt_expand_head term ~args ~substs k
    (* let *)
    | TT_let { bound = _; value; return = term }, _ ->
        let substs = Substs.push ~to_:value ~at_:substs substs in
        with_tt_expand_head term ~args ~substs k
    (* head *)
    | TT_rigid_var _, _ | TT_forall _, _ | TT_lambda _, [] | TT_string _, _ ->
        k term ~args ~substs

  (* TODO: avoid cyclical expansion *)
  (* TODO: avoid allocating term nodes *)
  (* TODO: short circuit when same variable on both sides *)
  let rec tt_equal ~level ~left ~left_substs ~right ~right_substs =
    match (left, right) with
    | TT_rigid_var { var = left_var }, TT_rigid_var { var = right_var }
      when Level.equal left_var right_var ->
        ()
    | left, right ->
        tt_equal ~level ~left:(tt_type_of left) ~left_substs
          ~right:(tt_type_of right) ~right_substs;
        tt_modulo_equal ~level ~left ~left_substs ~right ~right_substs

  and tt_modulo_equal ~level ~left ~left_substs ~right ~right_substs =
    with_tt_expand_head left ~args:[] ~substs:left_substs
    @@ fun left ~args:left_args ~substs:left_substs ->
    with_tt_expand_head right ~args:[] ~substs:right_substs
    @@ fun right ~args:right_args ~substs:right_substs ->
    tt_struct_equal ~level ~left ~left_substs ~right ~right_substs;
    (* TODO: arith clash *)
    List.iter2
      (fun (left, left_substs) (right, right_substs) ->
        tt_equal ~level ~left ~left_substs ~right ~right_substs)
      left_args right_args

  and tt_struct_equal ~level ~left ~left_substs ~right ~right_substs =
    match (left, right) with
    | TT_typed _, _ | _, TT_typed _ -> failwith "invariant"
    | TT_annot _, _ | _, TT_annot _ -> failwith "invariant"
    | TT_let _, _ | _, TT_let _ -> failwith "invariant"
    | TT_global_var _, _ | _, TT_global_var _ -> failwith "invariant"
    | TT_local_var _, _ | _, TT_local_var _ -> failwith "invariant"
    | TT_rigid_var { var = left }, TT_rigid_var { var = right }
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
    | ( (TT_rigid_var _ | TT_forall _ | TT_lambda _ | TT_apply _ | TT_string _),
        (TT_rigid_var _ | TT_forall _ | TT_lambda _ | TT_apply _ | TT_string _)
      ) ->
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
      tt_typed ~type_:left_type @@ tt_rigid_var ~var:level
    in
    let right_to_ =
      (* TODO: maybe pack right_type? *)
      tt_typed ~type_:right_type @@ tt_rigid_var ~var:level
    in
    let left_substs = Substs.push ~to_:left_to_ ~at_:left_substs left_substs in
    let right_substs =
      Substs.push ~to_:right_to_ ~at_:right_substs right_substs
    in
    k ~level ~left_substs ~right_substs

  (* TODO: linear functions can be faster without packing *)
  (* TODO: variable expansions without args can be faster without packing *)

  (* TODO: this is super hackish *)
  let rec tt_split_forall type_ ~args ~substs =
    match (type_, args) with
    | TT_forall { param; return }, [] ->
        let wrapped_arg_type = tp_type_of param in
        let apply_return_type ~arg =
          tt_typed ~type_:tt_rigid_univ
          @@ tt_let ~bound:param ~value:arg ~return
        in
        (wrapped_arg_type, apply_return_type)
    (* annot *)
    | TT_typed { term = type_; type_ = _ }, _ ->
        tt_split_forall type_ ~args ~substs
    | TT_annot { term = type_; annot = _ }, _ ->
        tt_split_forall type_ ~args ~substs
    (* substs *)
    | TT_global_var { var }, _ ->
        let type_, _at = Substs.find_global ~var substs in
        tt_split_forall type_ ~args ~substs
    | TT_local_var { var }, _ ->
        let type_, _at = Substs.find_local ~var substs in
        tt_split_forall type_ ~args ~substs
    (* beta *)
    | TT_lambda { param; return = type_ }, arg :: args ->
        tt_split_forall_subst ~bound:param ~value:arg type_ ~args ~substs
    | TT_apply { lambda = type_; arg }, _ ->
        let args =
          let size = Substs.size substs in
          tt_pack ~size arg :: args
        in
        tt_split_forall type_ ~args ~substs
    (* let *)
    | TT_let { bound; value; return = type_ }, _ ->
        let value =
          let size = Substs.size substs in
          tt_pack ~size value
        in
        tt_split_forall_subst ~bound ~value type_ ~args ~substs
    (* head *)
    | TT_rigid_var _, _ | TT_forall _, _ | TT_lambda _, [] | TT_string _, _ ->
        let type_ =
          List.fold_left
            (fun lambda arg ->
              tt_typed ~type_:tt_rigid_univ @@ tt_apply ~lambda ~arg)
            type_ args
        in
        error_not_a_forall ~type_

  and tt_split_forall_subst ~bound ~value type_ ~args ~substs =
    let substs = Substs.push ~to_:value ~at_:substs substs in
    let wrapped_arg_type, apply_return_type =
      tt_split_forall type_ ~args ~substs
    in
    let wrapped_arg_type =
      tt_typed ~type_:tt_rigid_univ
      @@ tt_let ~bound ~value ~return:wrapped_arg_type
    in
    let apply_return_type ~arg =
      tt_typed ~type_:tt_rigid_univ
      @@ tt_let ~bound ~value ~return:(apply_return_type ~arg)
    in
    (wrapped_arg_type, apply_return_type)

  let tt_split_forall type_ ~substs = tt_split_forall type_ ~args:[] ~substs
end

module Context : sig
  open Ttree

  type context

  val initial : context

  (* vars *)
  val lookup_var : context -> Name.t -> Level.t * term
  val with_bound_var : context -> Name.t -> type_:term -> (context -> 'k) -> 'k
  val with_subst_var : context -> Name.t -> to_:term -> (context -> 'k) -> 'k

  (* machinery *)
  val tt_equal : context -> left:term -> right:term -> unit
  val tt_split_forall : context -> term -> term * (arg:term -> term)
end = struct
  open Ttree
  open Terror

  type context = {
    level : Level.t;
    names : (Level.t * term) Name.Map.t;
    left_substs : Substs.t;
    right_substs : Substs.t;
  }

  let initial =
    (* TODO: this is bad *)
    let names =
      let open Name.Map in
      let names = empty in
      (* TODO: duplicated string name *)
      let names = add (Name.make "Type") (level_univ, tt_rigid_univ) names in
      let names =
        add (Name.make "String") (level_string, tt_rigid_univ) names
      in
      names
    in
    let level = level_string in
    let substs =
      let substs = Substs.empty in
      let substs =
        let univ =
          tt_typed ~type_:tt_rigid_univ @@ tt_rigid_var ~var:level_univ
        in
        Substs.push ~to_:univ ~at_:substs substs
      in
      let substs =
        let string =
          tt_typed ~type_:tt_rigid_univ @@ tt_rigid_var ~var:level_univ
        in
        Substs.push ~to_:string ~at_:substs substs
      in
      substs
    in
    let left_substs = substs in
    let right_substs = substs in
    { names; level; left_substs; right_substs }

  let lookup_var ctx name =
    match Name.Map.find_opt name ctx.names with
    | Some (var, type_) -> (var, type_)
    | None -> error_unknown_var ~name

  let with_bound_var ctx name ~type_ k =
    let { level; names; left_substs; right_substs } = ctx in
    let level = Level.next level in
    let type_ =
      (* TODO: is this a good place? *)
      (* TODO: substs should have the same size right? *)
      let size = Substs.size left_substs in
      Machinery.tt_pack ~size type_
    in
    let names = Name.Map.add name (level, type_) names in
    let to_ = tt_typed ~type_ @@ tt_rigid_var ~var:level in
    let left_substs = Substs.push ~to_ ~at_:left_substs left_substs in
    let right_substs = Substs.push ~to_ ~at_:right_substs right_substs in
    k @@ { level; names; left_substs; right_substs }

  let with_subst_var ctx name ~to_ k =
    let { level; names; left_substs; right_substs } = ctx in
    let to_ =
      (* TODO: is this a good place? *)
      let size = Substs.size left_substs in
      Machinery.tt_pack ~size to_
    in
    let level = Level.next level in
    let var = level in
    let names =
      let type_ = Machinery.tt_type_of to_ in
      Name.Map.add name (var, type_) names
    in
    let left_substs = Substs.push ~to_ ~at_:left_substs left_substs in
    let right_substs = Substs.push ~to_ ~at_:right_substs right_substs in
    k @@ { level; names; left_substs; right_substs }

  let tt_split_forall ctx type_ =
    let { level = _; names = _; left_substs; right_substs = _ } = ctx in
    (* TODO: left and right? *)
    Machinery.tt_split_forall type_ ~substs:left_substs

  let tt_equal ctx ~left ~right =
    let { level; names = _; left_substs; right_substs } = ctx in
    Machinery.tt_equal ~level ~left ~left_substs ~right ~right_substs
end

module Infer = struct
  open Ltree
  open Ttree
  open Terror
  open Machinery
  open Context

  (* TODO: does having expected_term also improves inference?
       Maybe with self and fix? But maybe not worth it
     Seems to help with many cases such as expected on annotation *)

  let rec infer_term ctx term =
    (* TODO: use this location *)
    let (LTerm { term; loc = _ }) = term in
    match term with
    | LT_annot { term; annot } ->
        (* TODO: expected term could propagate here *)
        let annot = check_annot ctx annot in
        (* TODO: unify annot before or after check term *)
        let term = check_term ctx term ~expected:annot in
        tt_annot ~term ~annot
    | LT_var { var = name } ->
        let var, type_ = lookup_var ctx name in
        tt_typed ~type_ @@ tt_global_var ~var
    | LT_extension _ -> error_extensions_not_implemented ()
    | LT_forall { param; return } ->
        with_infer_pat ctx param @@ fun ctx param ->
        let return = check_annot ctx return in
        tt_typed ~type_:tt_rigid_univ @@ tt_forall ~param ~return
    | LT_lambda { param; return } ->
        with_infer_pat ctx param @@ fun ctx param ->
        let return = infer_term ctx return in
        let type_ =
          let return = tt_type_of return in
          tt_typed ~type_:tt_rigid_univ @@ tt_forall ~param ~return
        in
        tt_typed ~type_ @@ tt_lambda ~param ~return
    | LT_apply { lambda; arg } ->
        let lambda = infer_term ctx lambda in
        (* TODO: this could be better? avoiding split forall? *)
        let wrapped_arg_type, apply_return_type =
          tt_split_forall ctx (tt_type_of lambda)
        in
        let arg = check_term ctx arg ~expected:wrapped_arg_type in
        let type_ = apply_return_type ~arg in
        tt_typed ~type_ @@ tt_apply ~lambda ~arg
    | LT_hoist _ -> error_hoist_not_implemented ()
    | LT_let { bound; value; return } ->
        (* TODO: use this loc *)
        let value = infer_term ctx value in
        (* TODO: this should be before value *)
        (* TODO: with_check_pat + subst  *)
        with_check_pat ctx bound ~expected:(tt_type_of value) ~to_:(Some value)
        @@ fun ctx bound ->
        let return = infer_term ctx return in
        let type_ =
          tt_typed ~type_:tt_rigid_univ
          @@ tt_let ~bound ~value ~return:(tt_type_of return)
        in
        tt_typed ~type_ @@ tt_let ~bound ~value ~return
    | LT_string { literal } ->
        tt_typed ~type_:tt_rigid_univ @@ tt_string ~literal

  and check_term ctx term ~expected =
    (* TODO: let () = assert_is_tt_with_type expected in *)
    (* TODO: expected should be a pattern? *)
    (* TODO: propagation through dependent things
        aka substitution inversion *)
    (* TODO: forall could in theory be improved by expected term *)
    (* TODO: apply could propagate when arg is var *)
    (* TODO: could propagate through let? *)
    let term = infer_term ctx term in
    let () =
      let received = tt_type_of term in
      tt_equal ctx ~left:received ~right:expected
    in
    term

  and check_annot ctx term = check_term ctx term ~expected:tt_rigid_univ

  and with_infer_pat ctx pat k =
    (* TODO: to_ here *)
    (* TODO: use this location *)
    let (LPat { pat; loc = _ }) = pat in
    match pat with
    | LP_var { var = _ } -> error_missing_annotation ()
    | LP_annot { pat; annot } ->
        let annot = check_annot ctx annot in
        with_check_pat ctx pat ~expected:annot ~to_:None @@ fun ctx pat ->
        k ctx @@ tp_annot ~pat ~annot

  and with_check_pat ctx pat ~expected ~to_ k =
    (* TODO: let () = assert_is_tt_with_type expected in *)
    (* TODO: expected should be a pattern, to achieve strictness *)
    (* TODO: use this location *)
    let (LPat { pat; loc = _ }) = pat in
    match pat with
    | LP_annot { pat; annot } ->
        let annot = check_annot ctx annot in
        let () = tt_equal ctx ~left:annot ~right:expected in
        with_check_pat ctx pat ~expected:annot ~to_ @@ fun ctx pat ->
        k ctx @@ tp_typed ~type_:expected @@ tp_annot ~pat ~annot
    | LP_var { var = name } -> (
        (* TODO: this is bad *)
        match to_ with
        | Some to_ ->
            (* TODO: expected == to_? *)
            with_subst_var ctx name ~to_ @@ fun ctx ->
            k ctx @@ tp_typed ~type_:expected @@ tp_var ~name
        | None ->
            with_bound_var ctx name ~type_:expected @@ fun ctx ->
            k ctx @@ tp_typed ~type_:expected @@ tp_var ~name)

  let infer_term term =
    match infer_term Context.initial term with
    | term -> Ok term
    | exception TError { error } -> Error error
end
