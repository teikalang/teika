open Utils
open Syntax

module Machinery = struct
  open Ttree
  open Terror
  (* TODO: cache normalization? *)

  let term_split_term term =
    match term with
    | TTerm { term; type_ } -> (term, type_)
    | TType { term } -> (term, tt_type_univ)

  let term_syntax_of term =
    match term with TTerm { term; type_ = _ } -> term | TType { term } -> term

  let pat_syntax_of pat = match pat with TPat { pat; type_ = _ } -> pat
  let pat_type_of pat = match pat with TPat { pat = _; type_ } -> type_

  module Shifts : sig
    type shifts
    type t = shifts

    val empty : shifts
    val shift : by_:int -> shifts -> shifts
    val lift : by_:int -> shifts -> shifts
    val apply : shifts -> var:Index.t -> Index.t
    val term_commit : shifts -> term -> term
    val pat_commit : shifts -> pat -> pat
  end = struct
    (* TODO: make those stack allocations *)
    (* TODO: absolute vs relative *)
    type shifts =
      | Nil
      | Shift of { by_ : int; next : shifts }
      | Lift of { by_ : int; next : shifts }

    type t = shifts

    let empty = Nil

    let shift ~by_ next =
      match next with
      | Shift { by_ = base; next } ->
          let by_ = by_ + base in
          Shift { by_; next }
      | (Nil | Lift _) as next -> Shift { by_; next }

    let lift ~by_ next =
      match next with
      | Nil -> Nil
      | Lift { by_ = base; next } ->
          let by_ = by_ + base in
          Lift { by_; next }
      | Shift _ as next -> Lift { by_; next }

    let rec apply shifts ~depth ~var =
      match Index.repr var < depth with
      | true -> var
      | false -> (
          match shifts with
          | Nil -> var
          | Shift { by_; next = shifts } ->
              let var = Index.shift var ~by_ in
              apply shifts ~depth ~var
          | Lift { by_; next = shifts } ->
              let depth = depth + by_ in
              apply shifts ~depth ~var)

    (* TODO: reduce allocations? Maybe cata over term? *)
    (* TODO: tag terms without bound variables, aka no shifting *)
    (* TODO: tag terms without binders *)
    (* TODO: tag terms locally closed *)
    (* TODO: if no var to shift was found, then avoid allocating *)
    let rec term_shift shifts ~depth term =
      match term with
      | TTerm { term; type_ } ->
          let type_ = term_shift shifts ~depth type_ in
          tterm ~type_ @@ term_syntax_shift shifts ~depth term
      | TType { term } -> ttype @@ term_syntax_shift shifts ~depth term

    and term_syntax_shift shifts ~depth term : term_syntax =
      match term with
      | TT_annot { term; annot } ->
          let annot = term_shift shifts ~depth annot in
          let term = term_shift shifts ~depth term in
          tt_annot ~term ~annot
      | TT_free_var { var = _ } as term -> term
      | TT_bound_var { var } ->
          let var = apply ~depth shifts ~var in
          tt_bound_var ~var
      | TT_forall { param; return } ->
          let param = pat_shift shifts ~depth param in
          let return =
            let depth = 1 + depth in
            term_shift shifts ~depth return
          in
          tt_forall ~param ~return
      | TT_lambda { param; return } ->
          let param = pat_shift shifts ~depth param in
          let return =
            let depth = 1 + depth in
            term_shift shifts ~depth return
          in
          tt_lambda ~param ~return
      | TT_apply { lambda; arg } ->
          let lambda = term_shift shifts ~depth lambda in
          let arg = term_shift shifts ~depth arg in
          tt_apply ~lambda ~arg
      | TT_let { bound; value; return } ->
          let bound = pat_shift shifts ~depth bound in
          let value = term_shift shifts ~depth value in
          let return =
            let depth = 1 + depth in
            term_shift shifts ~depth return
          in
          tt_let ~bound ~value ~return
      | TT_string { literal = _ } as term -> term

    and pat_shift shifts ~depth pat =
      let (TPat { pat; type_ }) = pat in
      let type_ = term_shift shifts ~depth type_ in
      tpat ~type_ @@ pat_syntax_shift shifts ~depth pat

    and pat_syntax_shift shifts ~depth pat =
      match pat with
      | TP_annot { pat; annot } ->
          let annot = term_shift shifts ~depth annot in
          let pat = pat_shift shifts ~depth pat in
          tp_annot ~annot ~pat
      | TP_var _ as pat -> pat

    let apply shifts ~var = apply shifts ~depth:0 ~var
    let term_commit shifts term = term_shift shifts ~depth:0 term
    let pat_commit shifts pat = pat_shift shifts ~depth:0 pat
  end

  module Substs : sig
    open Ttree

    type substs
    type t = substs

    val make : unit -> substs
    val push : pat -> to_:term -> at_:substs -> substs -> substs
    val apply : var:Index.t -> substs -> term * substs

    val capture :
      substs ->
      (substs -> 'a * substs) ->
      'a * [ `Wrap of term -> term ] * [ `Shift of term -> term ]

    val term_commit_shifts : substs -> term -> term * substs
  end = struct
    open Ttree

    type substs =
      | Substs of {
          next : Level.t;
          shifts : Shifts.t;
          (* TODO: benchmark  *)
          content : (pat * term * Shifts.t) array;
        }

    type t = substs

    let initial_tuple = (tpat_nil, tterm_nil, Shifts.empty)

    let make () =
      let initial_size = 256 in
      let content = Array.make initial_size initial_tuple in
      Substs { next = Level.zero; shifts = Shifts.empty; content }

    (* TODO: better name than at_ *)
    let push pat ~to_ ~at_ substs =
      let (Substs { next; shifts; content }) = substs in
      let content =
        (* resize *)
        let length = Array.length content in
        match Level.repr next >= length with
        | true ->
            (* TODO: limit memory usage *)
            (* TODO: maybe fail with out of memory? *)
            let new_content = Array.make (length * 2) initial_tuple in
            Array.blit content 0 new_content 0 length;
            new_content
        | false -> content
      in
      let () =
        let (Substs { next = at_; shifts; content = _ }) = at_ in
        let by_ = Level.repr next - Level.repr at_ in
        let shifts = Shifts.lift ~by_ shifts in
        let shifts = Shifts.shift ~by_ shifts in
        Array.set content (Level.repr next) (pat, to_, shifts)
      in
      let next = Level.next next in
      let shifts = Shifts.lift ~by_:1 shifts in
      Substs { next; shifts; content }

    let apply ~var substs =
      let (Substs { next; shifts; content }) = substs in
      (* TODO: proper errors here *)
      let var = Shifts.apply ~var shifts in
      let _pat, term, shifts =
        let from = Level.repr next - 1 - Index.repr var in
        Array.get content from
      in
      let shifts =
        let by_ = 1 + Index.repr var in
        Shifts.shift ~by_ shifts
      in
      let substs = Substs { next; shifts; content } in
      (term, substs)

    let term_commit_shifts substs term =
      (* TODO: very weird function  *)
      let (Substs { next; shifts; content }) = substs in
      let term = Shifts.term_commit shifts term in
      (term, Substs { next; shifts = Shifts.empty; content })

    let capture substs f =
      (* TODO: really ugly function *)
      let (Substs { next = from; shifts = _; content = _ }) = substs in
      let x, Substs { next = to_; shifts; content } = f substs in
      (* TODO: this is weird *)
      assert (shifts = Shifts.empty);
      let rec find_substs ~from acc =
        match Level.repr from < Level.repr to_ with
        | true ->
            let pat, term, shifts = Array.get content @@ Level.repr from in
            let term = Shifts.term_commit shifts term in
            let pat = Shifts.pat_commit shifts pat in
            let from = Level.next from in
            find_substs ~from ((pat, term) :: acc)
        | false -> acc
      in
      let rev_substs = find_substs ~from [] in
      let wrap term =
        (* TODO: better than let here *)
        (* TODO: this ttype here is really weird *)
        List.fold_left
          (fun return (bound, value) -> ttype @@ tt_let ~bound ~value ~return)
          term rev_substs
      in
      let shift term =
        let open Shifts in
        let by_ = Level.repr to_ - Level.repr from in
        let shifts = shift ~by_ empty in
        Shifts.term_commit shifts term
      in
      (x, `Wrap wrap, `Shift shift)
  end

  (* TODO: gas count *)
  let rec term_expand_head term ~args ~substs =
    match (term_syntax_of term, args) with
    | TT_annot { term; annot = _ }, _ ->
        (* annot *)
        term_expand_head term ~args ~substs
    | TT_bound_var { var }, _ ->
        (* subst *)
        let term, substs = Substs.apply ~var substs in
        term_expand_head term ~args ~substs
    | TT_apply { lambda; arg }, args ->
        (* beta1 *)
        let args = (arg, substs) :: args in
        term_expand_head lambda ~args ~substs
    | TT_lambda { param; return }, (arg, arg_substs) :: args ->
        (* beta2 *)
        let substs = Substs.push param ~to_:arg ~at_:arg_substs substs in
        term_expand_head return ~args ~substs
    | TT_let { bound; value; return }, _ ->
        (* zeta *)
        let substs = Substs.push bound ~to_:value ~at_:substs substs in
        term_expand_head return ~args ~substs
    | TT_free_var _, _ | TT_forall _, _ | TT_lambda _, [] | TT_string _, _ ->
        (* head *)
        (term, args, substs)

  (* TODO: avoid cyclical expansion *)
  (* TODO: avoid allocating term nodes *)
  (* TODO: short circuit when same variable on both sides *)
  let rec tt_equal ~level ~left ~left_substs ~right ~right_substs =
    let left, left_args, left_substs =
      term_expand_head left ~args:[] ~substs:left_substs
    in
    let right, right_args, right_substs =
      term_expand_head right ~args:[] ~substs:right_substs
    in
    tt_apply_equal ~level ~left ~left_args ~left_substs ~right ~right_args
      ~right_substs

  and tt_apply_equal ~level ~left ~left_args ~left_substs ~right ~right_args
      ~right_substs =
    tt_struct_equal ~level ~left ~left_substs ~right ~right_substs;
    (* TODO: arith clash *)
    List.iter2
      (fun (left, left_substs) (right, right_substs) ->
        tt_equal ~level ~left ~left_substs ~right ~right_substs)
      left_args right_args

  and tt_struct_equal ~level ~left ~left_substs ~right ~right_substs =
    match (term_syntax_of left, term_syntax_of right) with
    | TT_annot _, _ | _, TT_annot _ -> failwith "invariant"
    | TT_bound_var _, _ | _, TT_bound_var _ -> failwith "invariant"
    | TT_apply _, _ | _, TT_apply _ -> failwith "invariant"
    | TT_let _, _ | _, TT_let _ -> failwith "invariant"
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
    | TT_string { literal = left }, TT_string { literal = right }
      when String.equal left right ->
        ()
    | ( (TT_free_var _ | TT_forall _ | TT_lambda _ | TT_string _),
        (TT_free_var _ | TT_forall _ | TT_lambda _ | TT_string _) ) ->
        (* TODO: type clash needs substs *)
        error_type_clash ~left ~right

  and with_tp_equal_contra ~level ~left ~left_substs ~right ~right_substs k =
    (* TODO: contra? *)
    let left_type = pat_type_of left in
    let right_type = pat_type_of right in
    tt_equal ~level ~left:left_type ~left_substs ~right:right_type ~right_substs;
    let level = Level.next level in
    let left_to_ =
      (* TODO: maybe pack left_type? *)
      tterm ~type_:left_type @@ tt_free_var ~var:level
    in
    let right_to_ =
      (* TODO: maybe pack right_type? *)
      tterm ~type_:right_type @@ tt_free_var ~var:level
    in
    let left_substs =
      Substs.push left ~to_:left_to_ ~at_:left_substs left_substs
    in
    let right_substs =
      Substs.push right ~to_:right_to_ ~at_:right_substs right_substs
    in
    k ~level ~left_substs ~right_substs

  (* TODO: linear functions can be faster without packing *)
  (* TODO: variable expansions without args can be faster without packing *)

  (* TODO: this is super hackish *)
  let tt_split_forall type_ ~substs =
    let (type_, args), `Wrap wrap, `Shift shift =
      Substs.capture substs @@ fun substs ->
      let type_, args, substs = term_expand_head type_ ~args:[] ~substs in
      let type_, substs = Substs.term_commit_shifts substs type_ in
      ((type_, args), substs)
    in
    match (term_syntax_of type_, args) with
    | TT_forall { param; return }, [] ->
        let wrapped_arg_type = wrap @@ pat_type_of param in
        let apply_return_type ~arg =
          let arg = shift arg in
          (* TODO: weird ttype here *)
          wrap @@ ttype @@ tt_let ~bound:param ~value:arg ~return
        in
        (wrapped_arg_type, apply_return_type)
    (* head *)
    | TT_free_var _, _ | TT_forall _, _ | TT_lambda _, [] | TT_string _, _ ->
        (* TODO: dump substs *)
        error_not_a_forall ~type_
    | TT_annot _, _ -> failwith "invariant"
    | TT_bound_var _, _ -> failwith "invariant"
    | TT_lambda _, _ -> failwith "invariant"
    | TT_apply _, _ -> failwith "invariant"
    | TT_let _, _ -> failwith "invariant"
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
  open Ctree
  open Ttree
  open Elaborate_context
  open Terror

  (* TODO: does having expected_term also improves inference?
       Maybe with self and fix? But maybe not worth it
     Seems to help with many cases such as expected on annotation *)
  let tterm term =
    (* TODO: some of those could already be typed *)
    tterm ~type_:tterm_nil term

  let rec elaborate_term ctx term =
    (* TODO: use this location *)
    let (CTerm { term; loc = _ }) = term in
    match term with
    | CT_parens { content = term } -> elaborate_term ctx term
    | CT_annot { value = term; annot } ->
        let annot = elaborate_term ctx annot in
        let term = elaborate_term ctx term in
        tterm @@ tt_annot ~term ~annot
    | CT_var { var = name } ->
        let var = lookup_var ctx name in
        tterm @@ tt_bound_var ~var
    | CT_extension _ -> error_extensions_not_implemented ()
    | CT_forall { param; return } ->
        with_elaborate_pat ctx param @@ fun ctx param ->
        let return = elaborate_term ctx return in
        tterm @@ tt_forall ~param ~return
    | CT_lambda { param; return } ->
        with_elaborate_pat ctx param @@ fun ctx param ->
        let return = elaborate_term ctx return in
        tterm @@ tt_lambda ~param ~return
    | CT_apply { lambda; arg } ->
        let lambda = elaborate_term ctx lambda in
        let arg = elaborate_term ctx arg in
        tterm @@ tt_apply ~lambda ~arg
    | CT_semi { left; right } -> elaborate_semi ctx ~left ~right
    | CT_string { literal } -> tterm @@ tt_string ~literal
    | CT_pair _ | CT_both _ | CT_bind _ | CT_number _ | CT_braces _ ->
        error_invalid_notation ()

  and elaborate_semi ctx ~left ~right =
    let (CTerm { term = left; loc = _ }) = left in
    match left with
    | CT_parens { content = left } -> elaborate_semi ctx ~left ~right
    | CT_bind { bound; value } ->
        let value = elaborate_term ctx value in
        (* TODO: this should be before value *)
        with_elaborate_pat ctx bound @@ fun ctx bound ->
        let return = elaborate_term ctx right in
        tterm @@ tt_let ~bound ~value ~return
    | CT_annot { value = _; annot = _ } -> error_hoist_not_implemented ()
    | CT_var _ | CT_extension _ | CT_forall _ | CT_lambda _ | CT_apply _
    | CT_pair _ | CT_both _ | CT_semi _ | CT_string _ | CT_number _
    | CT_braces _ ->
        error_invalid_notation ()

  and with_elaborate_pat ctx pat k =
    (* TODO: to_ here *)
    (* TODO: use this location *)
    let (CTerm { term = pat; loc = _ }) = pat in
    let tp_typed pat =
      (* TODO: some of those could already be typed *)
      tpat ~type_:tterm_nil pat
    in
    match pat with
    | CT_parens { content = pat } -> with_elaborate_pat ctx pat k
    | CT_var { var = name } ->
        with_bound_var ctx name @@ fun ctx -> k ctx @@ tp_typed @@ tp_var ~name
    | CT_annot { value = pat; annot } ->
        let annot = elaborate_term ctx annot in
        with_elaborate_pat ctx pat @@ fun ctx pat ->
        k ctx @@ tp_typed @@ tp_annot ~pat ~annot
    | _ -> error_invalid_notation ()
end

module Infer_context : sig
  open Ttree

  type context

  val make_initial : unit -> context

  (* vars *)
  val find_var_type : context -> Index.t -> term
  val with_bound_var : context -> pat -> type_:term -> (context -> 'k) -> 'k
  val with_subst_var : context -> pat -> to_:term -> (context -> 'k) -> 'k

  (* machinery *)
  val tt_equal : context -> left:term -> right:term -> unit
  val tt_split_forall : context -> term -> term * (arg:term -> term)
end = struct
  open Ttree
  open Machinery

  type context = {
    level : Level.t;
    left_substs : Substs.t;
    right_substs : Substs.t;
  }

  let make_initial () =
    let level = level_string in
    let make_substs () =
      let substs = Substs.make () in
      let substs = Substs.push tpat_nil ~to_:tterm_nil ~at_:substs substs in
      (* TODO: better than tp_nil *)
      let substs = Substs.push tpat_nil ~to_:tt_type_univ ~at_:substs substs in
      let substs =
        Substs.push tpat_nil ~to_:tt_type_string ~at_:substs substs
      in
      substs
    in
    let left_substs = make_substs () in
    let right_substs = make_substs () in
    { level; left_substs; right_substs }

  let find_var_type ctx var =
    let { level = _; left_substs; right_substs = _ } = ctx in
    let term, substs = Substs.apply ~var left_substs in
    let _term, type_ = Machinery.term_split_term term in
    let type_, _substs = Substs.term_commit_shifts substs type_ in
    type_

  let with_bound_var ctx pat ~type_ k =
    let { level; left_substs; right_substs } = ctx in
    let level = Level.next level in
    let to_ = tterm ~type_ @@ tt_free_var ~var:level in
    let left_substs = Substs.push pat ~to_ ~at_:left_substs left_substs in
    let right_substs = Substs.push pat ~to_ ~at_:right_substs right_substs in
    k @@ { level; left_substs; right_substs }

  let with_subst_var ctx pat ~to_ k =
    let { level; left_substs; right_substs } = ctx in
    let level = Level.next level in
    let left_substs = Substs.push pat ~to_ ~at_:left_substs left_substs in
    let right_substs = Substs.push pat ~to_ ~at_:right_substs right_substs in
    k @@ { level; left_substs; right_substs }

  let tt_split_forall ctx type_ =
    let { level = _; left_substs; right_substs = _ } = ctx in
    (* TODO: left and right? *)
    let wrapped_arg_type, apply_return_type =
      tt_split_forall type_ ~substs:left_substs
    in
    (wrapped_arg_type, apply_return_type)

  let tt_equal ctx ~left ~right =
    let { level; left_substs; right_substs } = ctx in
    tt_equal ~level ~left ~left_substs ~right ~right_substs
end

module Infer = struct
  open Ttree
  open Terror
  open Machinery
  open Infer_context

  (* TODO: does having expected_term also improves inference?
       Maybe with self and fix? But maybe not worth it
     Seems to help with many cases such as expected on annotation *)

  let rec infer_term ctx term =
    match term with
    | TTerm typed ->
        let type_ = infer_term_syntax ctx term in
        typed.type_ <- type_;
        type_
    | TType { term = _ } -> failwith "unreachable"

  and infer_term_syntax ctx term =
    match term_syntax_of term with
    | TT_free_var { var = _ } -> failwith "unreachable"
    | TT_annot { term; annot } ->
        (* TODO: expected term could propagate here *)
        let () = check_annot ctx annot in
        (* TODO: unify annot before or after check term *)
        let () = check_term ctx term ~expected:annot in
        annot
    | TT_bound_var { var } -> find_var_type ctx var
    | TT_forall { param; return } ->
        with_infer_pat ctx param @@ fun ctx ->
        let () = check_annot ctx return in
        tt_type_univ
    | TT_lambda { param; return } ->
        with_infer_pat ctx param @@ fun ctx ->
        let return = infer_term ctx return in
        ttype @@ tt_forall ~param ~return
    | TT_apply { lambda; arg } ->
        let forall = infer_term ctx lambda in
        (* TODO: this could be better? avoiding split forall? *)
        let wrapped_arg_type, apply_return_type = tt_split_forall ctx forall in
        let () = check_term ctx arg ~expected:wrapped_arg_type in
        apply_return_type ~arg
    | TT_let { bound; value; return } ->
        (* TODO: use this loc *)
        let value_type = infer_term ctx value in
        (* TODO: this should be before value *)
        (* TODO: with_check_pat + subst  *)
        with_check_pat ctx bound ~expected:value_type ~to_:value @@ fun ctx ->
        let return_type = infer_term ctx return in
        ttype @@ tt_let ~bound ~value ~return:return_type
    | TT_string { literal = _ } -> tt_type_string

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
    with_bound_var ctx pat ~type_ k

  and infer_pat ctx pat =
    match pat with
    | TPat typed ->
        let type_ = infer_pat_syntax ctx pat in
        typed.type_ <- type_;
        type_

  and infer_pat_syntax ctx pat =
    match pat_syntax_of pat with
    | TP_var { name = _ } -> error_missing_annotation ()
    | TP_annot { pat; annot } ->
        let () = check_annot ctx annot in
        let () = check_pat ctx pat ~expected:annot in
        annot

  and with_check_pat ctx pat ~expected ~to_ k =
    let () = check_pat ctx pat ~expected in
    with_subst_var ctx pat ~to_ k

  and check_pat ctx pat ~expected =
    (* TODO: let () = assert_is_tt_with_type expected in *)
    (* TODO: expected should be a pattern, to achieve strictness *)
    let (TPat typed) = pat in
    typed.type_ <- expected;
    check_pat_syntax ctx pat ~expected

  and check_pat_syntax ctx pat ~expected =
    match pat_syntax_of pat with
    | TP_annot { pat; annot } ->
        let () = check_annot ctx annot in
        let () = tt_equal ctx ~left:annot ~right:expected in
        check_pat ctx pat ~expected:annot
    | TP_var { name = _ } -> ()

  let infer_term term =
    match
      let ctx = Infer_context.make_initial () in
      let term = Elaborate.elaborate_term Elaborate_context.initial term in
      infer_term ctx term
    with
    | term -> Ok term
    | exception TError { error } -> Error error
end
