open Utils
open Syntax

module Machinery = struct
  open Ttree
  open Terror

  let tt_syntax_of term =
    match term with TTerm { term; type_ = _ } -> term | TType { term } -> term

  let tt_type_of term =
    match term with
    | TTerm { term = _; type_ } -> type_
    | TType { term = _ } -> tt_global_univ

  let tp_type_of pat = match pat with TPat { pat = _; type_ } -> type_

  let rec tp_var_of pat =
    match pat with TPat { pat; type_ = _ } -> tp_syntax_var_of pat

  and tp_syntax_var_of pat =
    match pat with
    | TP_annot { pat; annot = _ } -> tp_var_of pat
    | TP_var { var } -> var

  let tv_fresh_of var =
    let (TVar { name; link = _; rename = _ }) = var in
    tv_fresh name

  (* TODO: accumulate rename, such that rename becomes amortized O(1) *)
  (* TODO: reduce allocations? Maybe cata over term? *)
  (* TODO: tag terms without bound variables, aka no rename *)
  (* TODO: lazy rename, rename + link *)
  let rec tt_rename term =
    match term with
    | TTerm { term; type_ } ->
        let term = tt_syntax_rename term in
        let type_ = tt_rename type_ in
        tterm ~type_ term
    | TType { term } ->
        let term = tt_syntax_rename term in
        ttype term

  and tt_syntax_rename term =
    match term with
    | TT_annot { term; annot } ->
        let term = tt_rename term in
        let annot = tt_rename annot in
        tt_annot ~term ~annot
    | TT_var { var } as term -> (
        match is_renamed var with
        | true ->
            let (TVar var) = var in
            tt_var ~var:var.rename
        | false -> term)
    | TT_forall { param; return } ->
        with_tp_rename param @@ fun param ->
        let return = tt_rename return in
        tt_forall ~param ~return
    | TT_lambda { param; return } ->
        with_tp_rename param @@ fun param ->
        let return = tt_rename return in
        tt_lambda ~param ~return
    | TT_apply { lambda; arg } ->
        let lambda = tt_rename lambda in
        let arg = tt_rename arg in
        tt_apply ~lambda ~arg
    | TT_hoist { bound; annot; return } ->
        with_tp_rename bound @@ fun bound ->
        let annot = tt_rename annot in
        let return = tt_rename return in
        tt_hoist ~bound ~annot ~return
    | TT_let { bound; value; return } ->
        with_tp_rename bound @@ fun bound ->
        let value = tt_rename value in
        let return = tt_rename return in
        tt_let ~bound ~value ~return
    | TT_string { literal } -> tt_string ~literal

  and with_tp_rename : type k. _ -> (_ -> k) -> k =
   fun pat k ->
    match pat with
    | TPat { pat; type_ } ->
        let type_ = tt_rename type_ in
        with_tp_syntax_rename pat @@ fun pat -> k @@ tpat ~type_ pat

  and with_tp_syntax_rename : type k. _ -> (_ -> k) -> k =
   fun pat k ->
    match pat with
    | TP_annot { pat; annot } ->
        (* TODO: should annot be lazily renamed? *)
        let annot = tt_rename annot in
        with_tp_rename pat @@ fun pat -> k @@ tp_annot ~pat ~annot
    | TP_var { var } ->
        let to_ = tv_fresh_of var in
        with_tv_rename var ~to_ @@ fun () -> k @@ tp_var ~var:to_

  (* TODO: avoid cyclical expansion *)
  (* TODO: avoid allocating term nodes *)
  (* TODO: gas count *)

  (* invariant, expand head returns head binders ready to link *)

  let rec with_tt_syntax_expand_head ~with_tp_subst term k =
    let with_tt_expand_head_and_substs term k =
      with_tt_expand_head_and_substs ~with_tp_subst term k
    in
    let with_tt_syntax_expand_head term k =
      with_tt_syntax_expand_head ~with_tp_subst term k
    in
    match term with
    | TT_annot { term; annot = _ } ->
        with_tt_syntax_expand_head (tt_syntax_of term) k
    | TT_var { var = _ } as term -> k term
    | TT_forall { param = _; return = _ } as term -> k term
    | TT_lambda { param = _; return = _ } as term -> k term
    (* beta *)
    | TT_apply { lambda; arg } -> (
        with_tt_expand_head_and_substs lambda @@ fun lambda ->
        match tt_syntax_of lambda with
        | TT_lambda { param; return } ->
            with_tp_subst param ~to_:arg @@ fun () ->
            with_tt_syntax_expand_head (tt_syntax_of return) k
        | _lambda ->
            (* TODO: preserve lambda? *)
            k @@ tt_apply ~lambda ~arg)
    (* zeta *)
    | TT_hoist { bound = _; annot = _; return } ->
        (* TODO: introduce bound? *)
        with_tt_syntax_expand_head (tt_syntax_of return) k
    | TT_let { bound; value; return } ->
        with_tp_subst bound ~to_:value @@ fun () ->
        with_tt_syntax_expand_head (tt_syntax_of return) k
    | TT_string { literal = _ } as term -> k term

  and with_tt_expand_head_and_substs ~with_tp_subst term k =
    let with_tt_syntax_expand_head_and_substs term k =
      with_tt_syntax_expand_head_and_substs ~with_tp_subst term k
    in
    match term with
    | TTerm { term; type_ } ->
        with_tt_syntax_expand_head_and_substs term @@ fun term ->
        k @@ tterm ~type_ term
    | TType { term } ->
        with_tt_syntax_expand_head_and_substs term @@ fun term ->
        k @@ ttype term

  and with_tt_syntax_expand_head_and_substs ~with_tp_subst term k =
    let with_tt_syntax_expand_head term k =
      with_tt_syntax_expand_head ~with_tp_subst term k
    in
    let with_tt_syntax_expand_head_and_substs term k =
      with_tt_syntax_expand_head_and_substs ~with_tp_subst term k
    in
    with_tt_syntax_expand_head term @@ function
    | TT_var { var } as term -> (
        match is_linked var with
        | true ->
            (* TODO: rename on register subst instead? *)
            let to_ =
              let (TVar var) = var in
              tt_rename var.link
            in
            with_tt_syntax_expand_head_and_substs (tt_syntax_of to_) k
        | false -> (
            match is_renamed var with
            | true ->
                let (TVar { name = _; link = _; rename = to_ }) = var in
                with_tt_syntax_expand_head_and_substs (tt_var ~var:to_) k
            | false -> k term))
    | ( TT_annot _ | TT_forall _ | TT_lambda _ | TT_apply _ | TT_hoist _
      | TT_let _ | TT_string _ ) as term ->
        k term

  (* TODO: duplicated code *)
  let rec with_tt_expand_head_and_substs ~with_tp_subst expanded term k =
    let with_tt_syntax_expand_head_and_substs expanded term k =
      with_tt_syntax_expand_head_and_substs ~with_tp_subst expanded term k
    in
    match term with
    | TTerm { term; type_ } ->
        with_tt_syntax_expand_head_and_substs expanded term
        @@ fun expanded term -> k expanded @@ tterm ~type_ term
    | TType { term } ->
        with_tt_syntax_expand_head_and_substs expanded term
        @@ fun expanded term -> k expanded @@ ttype term

  and with_tt_syntax_expand_head_and_substs ~with_tp_subst expanded term k =
    let with_tt_syntax_expand_head term k =
      with_tt_syntax_expand_head ~with_tp_subst term k
    in
    let with_tt_syntax_expand_head_and_substs expanded term k =
      with_tt_syntax_expand_head_and_substs ~with_tp_subst expanded term k
    in
    with_tt_syntax_expand_head term @@ function
    | TT_var { var } as term -> (
        match is_linked var with
        | true ->
            let to_ =
              let (TVar var) = var in
              tt_rename var.link
            in
            let expanded = var :: expanded in
            with_tt_syntax_expand_head_and_substs expanded (tt_syntax_of to_) k
        | false -> k expanded term)
    | ( TT_annot _ | TT_forall _ | TT_lambda _ | TT_apply _ | TT_hoist _
      | TT_let _ | TT_string _ ) as term ->
        k expanded term

  let with_tt_expand_head_and_substs ~with_tp_subst term k =
    with_tt_expand_head_and_substs ~with_tp_subst [] term k

  let rec with_tp_subst pat ~to_ k =
    match pat with
    | TPat { pat; type_ = _ } ->
        (* TODO: check type_ in debug mode *)
        with_tp_syntax_subst pat ~to_ k

  and with_tp_syntax_subst pat ~to_ k =
    match pat with
    | TP_annot { pat; annot = _ } ->
        (* TODO: check type_ in debug mode *)
        with_tp_subst pat ~to_ k
    | TP_var { var } -> with_tv_link var ~to_ k

  (* TODO: optimization, avoid expanding when left_lambda is equal to right_lambda *)
  (* TODO: short on physical equality *)
  let rec tt_equal ~left ~right =
    match (left, right) with
    | TType { term = _ }, TType { term = _ } -> tt_syntax_equal ~left ~right
    | ( TTerm { term = _; type_ = left_type },
        TTerm { term = _; type_ = right_type } ) ->
        tt_equal ~left:left_type ~right:right_type;
        tt_syntax_equal ~left ~right
    | TTerm { term = _; type_ = left_type }, TType { term = _ } ->
        tt_equal ~left:left_type ~right:tt_global_univ;
        tt_syntax_equal ~left ~right
    | TType { term = _ }, TTerm { term = _; type_ = right_type } ->
        tt_equal ~left:tt_global_univ ~right:right_type;
        tt_syntax_equal ~left ~right

  and tt_syntax_equal ~left ~right =
    with_tt_expand_head_and_substs ~with_tp_subst left
    @@ fun left_expanded left_head_normal ->
    with_tt_expand_head_and_substs ~with_tp_subst right
    @@ fun right_expanded right_head_normal ->
    Format.eprintf "left : %a\nright : %a\n%!" Tprinter.pp_term left
      Tprinter.pp_term right;
    let rec with_expanded_hypothesis expanded to_ k =
      match expanded with
      | [] -> k ()
      | var :: expanded ->
          Format.eprintf "var : %a; to_ : %a\n%!" Tprinter.pp_var var
            Tprinter.pp_term to_;
          with_force_tv_link var ~to_ @@ fun () ->
          with_expanded_hypothesis expanded to_ k
    in
    (* TODO: feels like this could oscillate between sides *)
    with_expanded_hypothesis left_expanded right @@ fun () ->
    with_expanded_hypothesis right_expanded left @@ fun () ->
    tt_syntax_equal_struct ~left:left_head_normal ~right:right_head_normal

  and tt_syntax_equal_struct ~left ~right =
    match (tt_syntax_of left, tt_syntax_of right) with
    | ( TT_annot { term = left_term; annot = left_annot },
        TT_annot { term = right_term; annot = right_annot } ) ->
        tt_equal ~left:left_annot ~right:right_annot;
        tt_equal ~left:left_term ~right:right_term
    | TT_var { var = var_left }, TT_var { var = var_right } -> (
        (* TODO: physical equality bad *)
        match var_left == var_right with
        | true -> ()
        | false -> error_type_clash ~left ~right)
    | ( TT_forall { param = left_param; return = left_return },
        TT_forall { param = right_param; return = right_return } ) ->
        with_tp_equal_contra ~left:left_param ~right:right_param @@ fun () ->
        tt_equal ~left:left_return ~right:right_return
    | ( TT_lambda { param = left_param; return = left_return },
        TT_lambda { param = right_param; return = right_return } ) ->
        with_tp_equal_contra ~left:left_param ~right:right_param @@ fun () ->
        tt_equal ~left:left_return ~right:right_return
    | ( TT_apply { lambda = left_lambda; arg = left_arg },
        TT_apply { lambda = right_lambda; arg = right_arg } ) ->
        tt_equal ~left:left_lambda ~right:right_lambda;
        tt_equal ~left:left_arg ~right:right_arg
    | TT_string { literal = left }, TT_string { literal = right } -> (
        match String.equal left right with
        | true -> ()
        | false -> error_string_clash ~left ~right)
    | ( TT_hoist { bound = left_bound; annot = left_annot; return = left_return },
        TT_hoist
          { bound = right_bound; annot = right_annot; return = right_return } )
      ->
        tt_equal ~left:left_annot ~right:right_annot;
        with_tp_equal_contra ~left:left_bound ~right:right_bound @@ fun () ->
        tt_equal ~left:left_return ~right:right_return
    | ( TT_let { bound = left_bound; value = left_value; return = left_return },
        TT_let
          { bound = right_bound; value = right_value; return = right_return } )
      ->
        tt_equal ~left:left_value ~right:right_value;
        with_tp_equal_contra ~left:left_bound ~right:right_bound @@ fun () ->
        tt_equal ~left:left_return ~right:right_return
    | ( ( TT_annot _ | TT_var _ | TT_forall _ | TT_lambda _ | TT_apply _
        | TT_string _ | TT_hoist _ | TT_let _ ),
        ( TT_annot _ | TT_var _ | TT_forall _ | TT_lambda _ | TT_apply _
        | TT_string _ | TT_hoist _ | TT_let _ ) ) ->
        error_type_clash ~left ~right

  and with_tp_equal_contra ~left ~right k =
    (* TODO: contra? *)
    let (TPat { pat = _; type_ = left_type }) = left in
    let (TPat { pat = _; type_ = right_type }) = right in
    tt_equal ~left:left_type ~right:right_type;
    (* alpha rename *)
    let left = tp_var_of left in
    let right = tp_var_of right in
    let skolem = tv_fresh_of right in
    let to_left = tterm ~type_:left_type @@ tt_var ~var:skolem in
    let to_right = tterm ~type_:right_type @@ tt_var ~var:skolem in
    with_tv_link left ~to_:to_left @@ fun () ->
    with_tv_link right ~to_:to_right @@ fun () -> k ()

  let tt_split_forall type_ =
    (* TODO: tt_subst? *)
    let with_tp_subst pat ~to_ k =
      let wrapped_arg_type, apply_return_type = with_tp_subst pat ~to_ k in
      let wrapped_arg_type =
        ttype @@ tt_let ~bound:pat ~value:to_ ~return:wrapped_arg_type
      in
      let apply_return_type ~arg =
        ttype @@ tt_let ~bound:pat ~value:to_ ~return:(apply_return_type ~arg)
      in
      (wrapped_arg_type, apply_return_type)
    in
    with_tt_expand_head_and_substs ~with_tp_subst type_
    @@ fun _expanded type_ ->
    match tt_syntax_of type_ with
    | TT_forall { param; return } ->
        let wrapped_arg_type = tp_type_of param in
        let apply_return_type ~arg =
          ttype @@ tt_let ~bound:param ~value:arg ~return
        in
        (wrapped_arg_type, apply_return_type)
    | _type_ -> error_not_a_forall ~type_
end

module Context : sig
  open Ttree

  type context

  val initial : context
  val lookup_var : context -> Name.t -> var * term
  val with_var : context -> Name.t -> type_:term -> (context -> var -> 'k) -> 'k
end = struct
  open Ttree
  open Terror

  type context = (var * term) Name.Map.t

  let enter_var ctx var ~type_ =
    let (TVar { name; link = _; rename = _ }) = var in
    Name.Map.add name (var, type_) ctx

  let initial =
    let ctx = Name.Map.empty in
    let ctx = enter_var ctx tv_univ ~type_:tt_global_univ in
    let ctx = enter_var ctx tv_string ~type_:tt_global_univ in
    ctx

  let lookup_var ctx name =
    match Name.Map.find_opt name ctx with
    | Some (var, type_) -> (var, type_)
    | None -> error_unknown_var ~name

  let with_var ctx name ~type_ k =
    let var = tv_fresh name in
    let ctx = enter_var ctx var ~type_ in
    k ctx var
end

module Infer = struct
  open Ltree
  open Ttree
  open Terror
  open Context
  open Machinery

  (* TODO: does having expected_term also improves inference?
       Maybe with self and fix? But maybe not worth it
     Seems to help with many cases such as expected on annotation *)

  let rec infer_term ctx term =
    match term with
    | LT_loc { term; loc = _ } ->
        (* TODO: use loc *)
        infer_term ctx term
    | LT_annot { term; annot } ->
        (* TODO: expected term could propagate here *)
        let annot = check_annot ctx annot in
        (* TODO: unify annot before or after check term *)
        let term = check_term ctx term ~expected:annot in
        tterm ~type_:annot @@ tt_annot ~term ~annot
    | LT_var { var = name } ->
        let var, type_ = lookup_var ctx name in
        let type_ = tt_rename type_ in
        tterm ~type_ @@ tt_var ~var
    | LT_extension _ -> error_extensions_not_implemented ()
    | LT_forall { param; return } ->
        with_infer_pat ctx param @@ fun ctx param ->
        let return = check_annot ctx return in
        ttype @@ tt_forall ~param ~return
    | LT_lambda { param; return } ->
        with_infer_pat ctx param @@ fun ctx param ->
        let return = infer_term ctx return in
        let type_ =
          let return = tt_type_of return in
          ttype @@ tt_forall ~param ~return
        in
        tterm ~type_ @@ tt_lambda ~param ~return
    | LT_apply { lambda; arg } ->
        let lambda = infer_term ctx lambda in
        (* TODO: this could be better? avoiding split forall? *)
        let wrapped_arg_type, apply_return_type =
          tt_split_forall (tt_type_of lambda)
        in
        let arg = check_term ctx arg ~expected:wrapped_arg_type in
        let type_ = apply_return_type ~arg in
        tterm ~type_ @@ tt_apply ~lambda ~arg
    | LT_hoist _ -> error_hoist_not_implemented ()
    | LT_let { bound; value; return } ->
        (* TODO: use this loc *)
        let value = infer_term ctx value in
        (* TODO: this should be before value *)
        (* TODO: with_check_pat + subst  *)
        with_check_pat ctx bound ~expected:(tt_type_of value)
        @@ fun ctx bound ->
        with_tp_subst bound ~to_:value @@ fun () ->
        let return = infer_term ctx return in
        let type_ = ttype @@ tt_let ~bound ~value ~return:(tt_type_of return) in
        tterm ~type_ @@ tt_let ~bound ~value ~return
    | LT_string { literal } ->
        tterm ~type_:tt_global_string @@ tt_string ~literal

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
      tt_equal ~left:received ~right:expected
    in
    term

  and check_annot ctx term = check_term ctx term ~expected:tt_global_univ

  and with_infer_pat ctx pat k =
    match pat with
    | LP_loc { pat; loc = _ } ->
        (* TODO: use this loc *)
        with_infer_pat ctx pat k
    | LP_var { var = _ } -> error_missing_annotation ()
    | LP_annot { pat; annot } ->
        (* TODO: TP_annot *)
        let annot = check_annot ctx annot in
        with_check_pat ctx pat ~expected:annot @@ fun ctx pat ->
        k ctx @@ tpat ~type_:annot @@ tp_annot ~pat ~annot

  and with_check_pat ctx pat ~expected k =
    (* TODO: let () = assert_is_tt_with_type expected in *)
    (* TODO: expected should be a pattern, to achieve strictness *)
    match pat with
    | LP_loc { pat; loc = _ } ->
        (* TODO: use this loc *)
        with_check_pat ctx pat ~expected k
    | LP_annot { pat; annot } ->
        let annot = check_annot ctx annot in
        let () = tt_equal ~left:annot ~right:expected in
        with_check_pat ctx pat ~expected:annot @@ fun ctx pat ->
        k ctx @@ tpat ~type_:expected @@ tp_annot ~pat ~annot
    | LP_var { var = name } ->
        with_var ctx name ~type_:expected @@ fun ctx var ->
        k ctx @@ tpat ~type_:expected @@ tp_var ~var

  let infer_term term =
    match infer_term Context.initial term with
    | term -> Ok term
    | exception TError { error } -> Error error
end
