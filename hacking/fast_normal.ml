(* TODO: document this *)

module Locally_nameless = struct
  type index = int

  type term =
    (* x *)
    | T_free_var of var
    (* \-n *)
    | T_bound_var of index
    (* λ. M *)
    | T_lambda of term
    (* M N *)
    | T_apply of term * term
    (* M[N] *)
    | T_subst of term * term

  and var = Var of unit

  let rec shift term ~depth ~by_ =
    match term with
    | T_free_var var -> T_free_var var
    | T_bound_var var -> (
        match var >= depth with
        | true -> T_bound_var (by_ + var)
        | false -> T_bound_var var)
    | T_lambda body ->
        let body = shift body ~depth:(1 + depth) ~by_ in
        T_lambda body
    | T_apply (lambda, arg) ->
        let lambda = shift lambda ~depth ~by_ in
        let arg = shift arg ~depth ~by_ in
        T_apply (lambda, arg)
    | T_subst (body, value) ->
        let value = shift value ~depth ~by_ in
        let body = shift body ~depth:(1 + depth) ~by_ in
        T_subst (body, value)

  module Substs = Map.Make (Int)

  let length substs =
    let length, _map = substs in
    length

  let subst ~to_ ~at_ substs =
    let length, map = substs in
    let map = Substs.add length (to_, at_) map in
    (1 + length, map)

  let expand_var var ~substs =
    match
      let length, map = substs in
      Substs.find_opt (length - 1 - var) map
    with
    | Some (to_, at_) ->
        let by_ = length substs - at_ in
        shift to_ ~depth:0 ~by_
    | None -> failwith "malformed term"

  let rec expand_head term ~args ~substs k =
    match (term, args) with
    (* subst *)
    | T_bound_var var, _ ->
        let term = expand_var var ~substs in
        expand_head term ~args ~substs k
    (* zeta *)
    | T_subst (body, value), _ ->
        let substs =
          let at_ = length substs in
          subst ~to_:value ~at_ substs
        in
        expand_head body ~args ~substs k
    (* beta *)
    | T_lambda body, (arg, arg_substs) :: args ->
        let substs =
          let at_ = length arg_substs in
          subst ~to_:arg ~at_ substs
        in
        expand_head body ~args ~substs k
    | T_apply (lambda, arg), _ ->
        let args = (arg, substs) :: args in
        expand_head lambda ~args ~substs k
    (* head *)
    | T_free_var _, _ | T_lambda _, [] -> k term ~args ~substs

  let rec equal left ~left_substs right ~right_substs =
    expand_head left ~args:[] ~substs:left_substs
    @@ fun left ~args:left_args ~substs:left_substs ->
    expand_head right ~args:[] ~substs:right_substs
    @@ fun right ~args:right_args ~substs:right_substs ->
    equal_struct left ~left_substs right ~right_substs;
    (* TODO: clash *)
    List.iter2
      (fun (left, left_substs) (right, right_substs) ->
        equal left ~left_substs right ~right_substs)
      left_args right_args

  and equal_struct left ~left_substs right ~right_substs =
    match (left, right) with
    | T_bound_var _, _ | _, T_bound_var _ -> failwith "unreachable bound"
    | T_apply (_, _), _ | _, T_apply (_, _) -> failwith "unreachable apply"
    | T_subst _, _ | _, T_subst _ -> failwith "unreachable subst"
    | T_free_var left, T_free_var right when left == right -> ()
    | T_lambda left_body, T_lambda right_body ->
        let skolem = T_free_var (Var ()) in
        let left_substs =
          let at_ = length left_substs in
          subst ~to_:skolem ~at_ left_substs
        in
        let right_substs =
          let at_ = length right_substs in
          subst ~to_:skolem ~at_ right_substs
        in
        equal left_body ~left_substs right_body ~right_substs
    | _ -> failwith "clash"

  let rec of_string env ~level term =
    let open Utils in
    let open Syntax.Ltree in
    let (LTerm { term; loc = _ }) = term in
    match term with
    | LT_var { var } -> (
        match Name.Map.find_opt var env with
        | Some at_ -> T_bound_var (level - at_)
        | None -> failwith @@ Format.asprintf "missing variable %a" Name.pp var)
    | LT_extension _ -> failwith "not supported"
    | LT_forall _ -> failwith "not supported"
    | LT_lambda { param; return } ->
        with_of_string_pat env ~level param @@ fun env ~level ->
        let return = of_string env ~level return in
        T_lambda return
    | LT_apply { lambda; arg } ->
        let lambda = of_string env ~level lambda in
        let arg = of_string env ~level arg in
        T_apply (lambda, arg)
    | LT_hoist _ -> failwith "not supported"
    | LT_let { bound; value; return } ->
        let value = of_string env ~level value in
        with_of_string_pat env ~level bound @@ fun env ~level ->
        let return = of_string env ~level return in
        T_subst (return, value)
    | LT_annot _ -> failwith "not supported"
    | LT_string _ -> failwith "not supported"

  and with_of_string_pat env ~level pat k =
    let open Utils in
    let open Syntax.Ltree in
    let (LPat { pat; loc = _ }) = pat in
    match pat with
    | LP_var { var } ->
        let level = 1 + level in
        let env = Name.Map.add var level env in
        k env ~level
    | LP_annot _ -> failwith "not supported"

  let of_string code =
    let open Utils in
    let open Syntax in
    let code =
      Lparser.parse_term @@ Option.get
      @@ Clexer.from_string Cparser.term_opt code
    in
    of_string Name.Map.empty ~level:0 code
end

module Ln_but_stacks = struct
  type index = int

  type term = Locally_nameless.term =
    (* \n *)
    | T_free_var of var
    (* \-n *)
    | T_bound_var of index
    (* λ. M *)
    | T_lambda of term
    (* M N *)
    | T_apply of term * term
    (* M[N] *)
    | T_subst of term * term

  and var = Locally_nameless.var = Var of unit

  module Substs : sig
    type substs

    val make : unit -> substs
    val last : length:int -> substs -> int -> term * int
    val push : length:int -> substs -> term -> int -> int
  end = struct
    type substs = { terms : term array; positions : int array }

    let nil = T_free_var (Var ())

    let make () =
      let max_length = 512 * 1024 in
      let terms = Array.make max_length nil in
      let positions = Array.make max_length 0 in
      { terms; positions }

    let push ~length { terms; positions } value at_ =
      Array.set terms length value;
      Array.set positions length at_;
      1 + length

    let last ~length { terms; positions } offset =
      let index = length - 1 - offset in
      (Array.get terms index, Array.get positions index)
  end

  let rec shift term ~depth ~by_ =
    match term with
    | T_free_var var -> T_free_var var
    | T_bound_var var -> (
        match var >= depth with
        | true -> T_bound_var (by_ + var)
        | false -> T_bound_var var)
    | T_lambda body ->
        let body = shift body ~depth:(1 + depth) ~by_ in
        T_lambda body
    | T_apply (lambda, arg) ->
        let lambda = shift lambda ~depth ~by_ in
        let arg = shift arg ~depth ~by_ in
        T_apply (lambda, arg)
    | T_subst (body, value) ->
        let value = shift value ~depth ~by_ in
        let body = shift body ~depth:(1 + depth) ~by_ in
        T_subst (body, value)

  let shift term ~depth ~by_ = shift term ~depth ~by_

  let rec expand_head term ~args ~length ~substs =
    match (term, args) with
    (* subst *)
    | T_bound_var var, _ ->
        let term, at_ = Substs.last ~length substs var in
        let term =
          let by_ = length - at_ in
          shift term ~depth:0 ~by_
        in
        expand_head term ~args ~length ~substs
    (* zeta *)
    | T_subst (body, value), _ ->
        let length = Substs.push ~length substs value length in
        expand_head body ~args ~length ~substs
    (* beta *)
    | T_lambda body, (arg, at_) :: args ->
        let length = Substs.push ~length substs arg at_ in
        expand_head body ~args ~length ~substs
    | T_apply (lambda, arg), _ ->
        let args = (arg, length) :: args in
        expand_head lambda ~args ~length ~substs
    (* head *)
    | T_free_var _, _ | T_lambda _, [] -> (term, args, length, substs)

  let rec equal left ~left_length ~left_substs right ~right_length ~right_substs
      =
    let left, left_args, left_length, left_substs =
      expand_head left ~args:[] ~length:left_length ~substs:left_substs
    in
    let right, right_args, right_length, right_substs =
      expand_head right ~args:[] ~length:right_length ~substs:right_substs
    in
    equal_struct left ~left_length ~left_substs right ~right_length
      ~right_substs;
    (* TODO: clash *)
    List.iter2
      (fun (left, left_at) (right, right_at) ->
        let left =
          let by_ = left_length - left_at in
          shift left ~depth:0 ~by_
        in
        let right =
          let by_ = right_length - right_at in
          shift right ~depth:0 ~by_
        in
        equal left ~left_length ~left_substs right ~right_length ~right_substs)
      left_args right_args

  and equal_struct left ~left_length ~left_substs right ~right_length
      ~right_substs =
    match (left, right) with
    | T_bound_var _, _ | _, T_bound_var _ -> failwith "unreachable bound"
    | T_apply (_, _), _ | _, T_apply (_, _) -> failwith "unreachable apply"
    | T_subst _, _ | _, T_subst _ -> failwith "unreachable subst"
    | T_free_var left, T_free_var right when left == right -> ()
    | T_lambda left_body, T_lambda right_body ->
        let skolem = T_free_var (Var ()) in
        let left_length =
          Substs.push ~length:left_length left_substs skolem left_length
        in
        let right_length =
          Substs.push ~length:right_length right_substs skolem right_length
        in
        equal left_body ~left_length ~left_substs right_body ~right_length
          ~right_substs
    | _ -> failwith "clash"
end

module Ln_but_stacks_with_shifts = struct
  type index = int

  type term = Locally_nameless.term =
    (* \n *)
    | T_free_var of var
    (* \-n *)
    | T_bound_var of index
    (* λ. M *)
    | T_lambda of term
    (* M N *)
    | T_apply of term * term
    (* M[N] *)
    | T_subst of term * term

  and var = Locally_nameless.var = Var of unit

  type shifts = Nil | Shift of { by_ : int; at_ : int; next : shifts }

  let shift ~from ~length ~shifts =
    let by_ = length - from in
    Shift { by_; at_ = length; next = shifts }

  module Substs : sig
    type substs

    val make : unit -> substs
    val last : length:int -> substs -> int -> term * int * shifts
    val push : length:int -> substs -> term -> int -> shifts -> int
  end = struct
    type substs = {
      terms : term array;
      positions : int array;
      shifts : shifts array;
    }

    let nil = T_free_var (Var ())

    let make () =
      let max_length = 512 * 1024 in
      let terms = Array.make max_length nil in
      let positions = Array.make max_length 0 in
      let shifts = Array.make max_length Nil in
      { terms; positions; shifts }

    let push ~length { terms; positions; shifts } value at_ at_shifts =
      Array.set terms length value;
      Array.set positions length at_;
      Array.set shifts length at_shifts;
      1 + length

    let last ~length { terms; positions; shifts } offset =
      let index = length - 1 - offset in
      (Array.get terms index, Array.get positions index, Array.get shifts index)
  end

  let rec expand_var var ~length ~shifts =
    match shifts with
    | Nil -> var
    | Shift { by_; at_; next = shifts } -> (
        let distance =
          (* TODO: should this length decreases? *)
          length - at_
        in
        match var >= distance with
        | true ->
            let var = by_ + var in
            expand_var var ~length ~shifts
        | false -> var)

  let rec expand_head term ~args ~length ~shifts ~substs =
    match (term, args) with
    (* subst *)
    | T_bound_var var, _ ->
        let var = expand_var var ~length ~shifts in
        let term, from, shifts = Substs.last ~length substs var in
        let shifts = shift ~from ~length ~shifts in
        expand_head term ~args ~length ~shifts ~substs
    (* zeta *)
    | T_subst (body, value), _ ->
        let length = Substs.push ~length substs value length shifts in
        expand_head body ~args ~length ~shifts ~substs
    (* beta *)
    | T_lambda body, (arg, at_, arg_shifts) :: args ->
        let length = Substs.push ~length substs arg at_ arg_shifts in
        expand_head body ~args ~length ~shifts ~substs
    | T_apply (lambda, arg), _ ->
        let args = (arg, length, shifts) :: args in
        expand_head lambda ~args ~length ~shifts ~substs
    (* head *)
    | T_free_var _, _ | T_lambda _, [] -> (term, args, length, shifts, substs)

  let rec equal left ~left_length ~left_shifts ~left_substs right ~right_length
      ~right_shifts ~right_substs =
    let left, left_args, left_length, left_shifts, left_substs =
      expand_head left ~args:[] ~length:left_length ~shifts:left_shifts
        ~substs:left_substs
    in
    let right, right_args, right_length, right_shifts, right_substs =
      expand_head right ~args:[] ~length:right_length ~shifts:right_shifts
        ~substs:right_substs
    in
    equal_struct left ~left_length ~left_shifts ~left_substs right ~right_length
      ~right_shifts ~right_substs;
    (* TODO: clash *)
    List.iter2
      (fun (left, left_from, left_shifts) (right, right_from, right_shifts) ->
        let left_shifts =
          shift ~from:left_from ~length:left_length ~shifts:left_shifts
        in
        let right_shifts =
          shift ~from:right_from ~length:right_length ~shifts:right_shifts
        in
        equal left ~left_length ~left_shifts ~left_substs right ~right_length
          ~right_shifts ~right_substs)
      left_args right_args

  and equal_struct left ~left_length ~left_shifts ~left_substs right
      ~right_length ~right_shifts ~right_substs =
    match (left, right) with
    | T_bound_var _, _ | _, T_bound_var _ -> failwith "unreachable bound"
    | T_apply (_, _), _ | _, T_apply (_, _) -> failwith "unreachable apply"
    | T_subst _, _ | _, T_subst _ -> failwith "unreachable subst"
    | T_free_var left, T_free_var right when left == right -> ()
    | T_lambda left_body, T_lambda right_body ->
        let skolem = T_free_var (Var ()) in
        let left_length =
          Substs.push ~length:left_length left_substs skolem left_length Nil
        in
        let right_length =
          Substs.push ~length:right_length right_substs skolem right_length Nil
        in
        equal left_body ~left_length ~left_shifts ~left_substs right_body
          ~right_length ~right_shifts ~right_substs
    | _ -> failwith "clash"
end

module Ln_but_stacks_with_shifts_packed = struct
  type index = int

  type term = Locally_nameless.term =
    (* \n *)
    | T_free_var of var
    (* \-n *)
    | T_bound_var of index
    (* λ. M *)
    | T_lambda of term
    (* M N *)
    | T_apply of term * term
    (* M[N] *)
    | T_subst of term * term

  and var = Locally_nameless.var = Var of unit

  type shifts = Nil | Shift of { by_ : int; at_ : int; next : shifts }

  let shift ~from ~length ~shifts =
    let by_ = length - from in
    Shift { by_; at_ = length; next = shifts }

  module Substs : sig
    type substs

    val make : unit -> substs
    val clear : substs -> unit
    val length : substs -> int

    (* *)
    val shifts : substs -> shifts
    val load_shifts : substs -> shifts -> unit

    (* *)
    val apply : substs -> var:int -> term
    val push : substs -> term -> int -> shifts -> unit
  end = struct
    type substs = {
      mutable length : int;
      mutable shifts : shifts;
      terms : term array;
      positions : int array;
      terms_shifts : shifts array;
    }

    let nil = T_free_var (Var ())

    let make () =
      let max_length = 1024 * 1024 in
      let terms = Array.make max_length nil in
      let positions = Array.make max_length 0 in
      let terms_shifts = Array.make max_length Nil in
      { length = 0; shifts = Nil; terms; positions; terms_shifts }

    let clear substs =
      substs.length <- 0;
      substs.shifts <- Nil

    let length substs = substs.length
    let shifts substs = substs.shifts
    let load_shifts substs shifts = substs.shifts <- shifts

    let push substs value at_ at_shifts =
      let { length; shifts = _; terms; positions; terms_shifts } = substs in
      Array.set terms length value;
      Array.set positions length at_;
      Array.set terms_shifts length at_shifts;
      substs.length <- 1 + length

    let rec apply var ~length ~shifts =
      match shifts with
      | Nil -> var
      | Shift { by_; at_; next = shifts } -> (
          (* TODO: should this length decreases? *)
          let distance = length - at_ in
          match var >= distance with
          | true ->
              let var = by_ + var in
              apply var ~length ~shifts
          | false -> var)

    let apply substs ~var =
      let { length; shifts; terms; positions; terms_shifts } = substs in
      let offset = apply var ~length ~shifts in
      let index = length - 1 - offset in
      let term = Array.get terms index in
      let from = Array.get positions index in
      let () =
        let shifts = Array.get terms_shifts index in
        substs.shifts <- shift ~from ~length ~shifts
      in
      term
  end

  let rec expand_head term ~args ~substs =
    match (term, args) with
    (* subst *)
    | T_bound_var var, _ ->
        let term = Substs.apply substs ~var in
        expand_head term ~args ~substs
    (* zeta *)
    | T_subst (body, value), _ ->
        let () =
          let length = Substs.length substs in
          let shifts = Substs.shifts substs in
          Substs.push substs value length shifts
        in
        expand_head body ~args ~substs
    (* beta *)
    | T_lambda body, (arg, at_, arg_shifts) :: args ->
        let () = Substs.push substs arg at_ arg_shifts in
        expand_head body ~args ~substs
    | T_apply (lambda, arg), _ ->
        let args =
          let length = Substs.length substs in
          let shifts = Substs.shifts substs in
          (arg, length, shifts) :: args
        in
        expand_head lambda ~args ~substs
    (* head *)
    | T_free_var _, _ | T_lambda _, [] -> (term, args, substs)

  let rec equal left ~left_substs right ~right_substs =
    let left, left_args, left_substs =
      expand_head left ~args:[] ~substs:left_substs
    in
    let right, right_args, right_substs =
      expand_head right ~args:[] ~substs:right_substs
    in
    equal_struct left ~left_substs right ~right_substs;
    (* TODO: clash *)
    List.iter2
      (fun (left, left_from, left_shifts) (right, right_from, right_shifts) ->
        let left_shifts =
          let length = Substs.length left_substs in
          shift ~from:left_from ~length ~shifts:left_shifts
        in
        let () = Substs.load_shifts left_substs left_shifts in
        let right_shifts =
          let length = Substs.length right_substs in
          shift ~from:right_from ~length ~shifts:right_shifts
        in
        let () = Substs.load_shifts right_substs right_shifts in
        equal left ~left_substs right ~right_substs)
      left_args right_args

  and equal_struct left ~left_substs right ~right_substs =
    match (left, right) with
    | T_bound_var _, _ | _, T_bound_var _ -> failwith "unreachable bound"
    | T_apply (_, _), _ | _, T_apply (_, _) -> failwith "unreachable apply"
    | T_subst _, _ | _, T_subst _ -> failwith "unreachable subst"
    | T_free_var left, T_free_var right when left == right -> ()
    | T_lambda left_body, T_lambda right_body ->
        let skolem = T_free_var (Var ()) in
        let () =
          let length = Substs.length left_substs in
          Substs.push left_substs skolem length Nil
        in
        let () =
          let length = Substs.length right_substs in
          Substs.push right_substs skolem length Nil
        in
        equal left_body ~left_substs right_body ~right_substs
    | _ -> failwith "clash"
end

module Ln_but_stacks_with_bounded_shifts = struct
  type index = int

  type term = Locally_nameless.term =
    (* \n *)
    | T_free_var of var
    (* \-n *)
    | T_bound_var of index
    (* λ. M *)
    | T_lambda of term
    (* M N *)
    | T_apply of term * term
    (* M[N] *)
    | T_subst of term * term

  and var = Locally_nameless.var = Var of unit

  type shifts =
    | Nil
    | Shift of { by_ : int; at_ : int; size : int; next : shifts }

  let size shifts =
    match shifts with
    | Nil -> 0
    | Shift { by_ = _; at_ = _; size; next = _ } -> size

  let shift ~from ~length ~shifts =
    let by_ = length - from in
    Shift { by_; at_ = length; size = 1 + size shifts; next = shifts }

  let rec expand_var var ~length ~shifts =
    match shifts with
    | Nil -> var
    | Shift { by_; at_; size = _; next = shifts } -> (
        let distance =
          (* TODO: should this length decreases? *)
          length - at_
        in
        match var >= distance with
        | true ->
            let var = by_ + var in
            expand_var var ~length ~shifts
        | false -> var)

  let rec commit_shift term ~length ~shifts =
    match term with
    | T_free_var var -> T_free_var var
    | T_bound_var var ->
        let var = expand_var var ~length ~shifts in
        T_bound_var var
    | T_lambda body ->
        let body = commit_shift body ~length:(1 + length) ~shifts in
        T_lambda body
    | T_apply (lambda, arg) ->
        let lambda = commit_shift lambda ~length ~shifts in
        let arg = commit_shift arg ~length ~shifts in
        T_apply (lambda, arg)
    | T_subst (body, value) ->
        let value = commit_shift value ~length ~shifts in
        let body = commit_shift body ~length:(1 + length) ~shifts in
        T_subst (body, value)

  let commit_shift term ~length ~shifts =
    match size shifts > 4 with
    | true -> (commit_shift term ~length ~shifts, Nil)
    | false -> (term, shifts)

  module Substs : sig
    type substs

    val make : unit -> substs
    val last : length:int -> substs -> int -> term * int * shifts
    val push : length:int -> substs -> term -> int -> shifts -> int
  end = struct
    type substs = {
      terms : term array;
      positions : int array;
      shifts : shifts array;
    }

    let nil = T_free_var (Var ())

    let make () =
      let max_length = 512 * 1024 in
      let terms = Array.make max_length nil in
      let positions = Array.make max_length 0 in
      let shifts = Array.make max_length Nil in
      { terms; positions; shifts }

    let push ~length { terms; positions; shifts } value at_ at_shifts =
      Array.set terms length value;
      Array.set positions length at_;
      Array.set shifts length at_shifts;
      1 + length

    let last ~length { terms; positions; shifts } offset =
      let index = length - 1 - offset in
      (Array.get terms index, Array.get positions index, Array.get shifts index)
  end

  let rec expand_head term ~args ~length ~shifts ~substs =
    match (term, args) with
    (* subst *)
    | T_bound_var var, _ ->
        let var = expand_var var ~length ~shifts in
        let term, from, shifts = Substs.last ~length substs var in
        let shifts = shift ~from ~length ~shifts in
        expand_head term ~args ~length ~shifts ~substs
    (* zeta *)
    | T_subst (body, value), _ ->
        let length = Substs.push ~length substs value length shifts in
        expand_head body ~args ~length ~shifts ~substs
    (* beta *)
    | T_lambda body, (arg, at_, arg_shifts) :: args ->
        let length = Substs.push ~length substs arg at_ arg_shifts in
        expand_head body ~args ~length ~shifts ~substs
    | T_apply (lambda, arg), _ ->
        let args = (arg, length, shifts) :: args in
        expand_head lambda ~args ~length ~shifts ~substs
    (* head *)
    | T_free_var _, _ | T_lambda _, [] -> (term, args, length, shifts, substs)

  let rec equal left ~left_length ~left_shifts ~left_substs right ~right_length
      ~right_shifts ~right_substs =
    let left, left_args, left_length, left_shifts, left_substs =
      expand_head left ~args:[] ~length:left_length ~shifts:left_shifts
        ~substs:left_substs
    in
    let right, right_args, right_length, right_shifts, right_substs =
      expand_head right ~args:[] ~length:right_length ~shifts:right_shifts
        ~substs:right_substs
    in
    equal_struct left ~left_length ~left_shifts ~left_substs right ~right_length
      ~right_shifts ~right_substs;
    (* TODO: clash *)
    List.iter2
      (fun (left, left_from, left_shifts) (right, right_from, right_shifts) ->
        let left_shifts =
          shift ~from:left_from ~length:left_length ~shifts:left_shifts
        in
        let right_shifts =
          shift ~from:right_from ~length:right_length ~shifts:right_shifts
        in
        let left, left_shifts =
          commit_shift left ~length:left_length ~shifts:left_shifts
        in
        let right, right_shifts =
          commit_shift right ~length:right_length ~shifts:right_shifts
        in
        equal left ~left_length ~left_shifts ~left_substs right ~right_length
          ~right_shifts ~right_substs)
      left_args right_args

  and equal_struct left ~left_length ~left_shifts ~left_substs right
      ~right_length ~right_shifts ~right_substs =
    match (left, right) with
    | T_bound_var _, _ | _, T_bound_var _ -> failwith "unreachable bound"
    | T_apply (_, _), _ | _, T_apply (_, _) -> failwith "unreachable apply"
    | T_subst _, _ | _, T_subst _ -> failwith "unreachable subst"
    | T_free_var left, T_free_var right when left == right -> ()
    | T_lambda left_body, T_lambda right_body ->
        let skolem = T_free_var (Var ()) in
        let left_length =
          Substs.push ~length:left_length left_substs skolem left_length Nil
        in
        let right_length =
          Substs.push ~length:right_length right_substs skolem right_length Nil
        in
        equal left_body ~left_length ~left_shifts ~left_substs right_body
          ~right_length ~right_shifts ~right_substs
    | _ -> failwith "clash"
end

module Ln_but_normal = struct
  type index = int

  type term = Locally_nameless.term =
    (* \n *)
    | T_free_var of var
    (* \-n *)
    | T_bound_var of index
    (* λ. M *)
    | T_lambda of term
    (* M N *)
    | T_apply of term * term
    (* M[N] *)
    | T_subst of term * term

  and var = Locally_nameless.var = Var of unit

  let rec shift term ~depth ~by_ =
    match term with
    | T_free_var var -> T_free_var var
    | T_bound_var var -> (
        match var >= depth with
        | true -> T_bound_var (by_ + var)
        | false -> T_bound_var var)
    | T_lambda body ->
        let body = shift body ~depth:(1 + depth) ~by_ in
        T_lambda body
    | T_apply (lambda, arg) ->
        let lambda = shift lambda ~depth ~by_ in
        let arg = shift arg ~depth ~by_ in
        T_apply (lambda, arg)
    | T_subst (body, value) ->
        let value = shift value ~depth ~by_ in
        let body = shift body ~depth:(1 + depth) ~by_ in
        T_subst (body, value)

  let shift term ~depth ~by_ = shift term ~depth ~by_

  module Substs = Map.Make (Int)

  let nil = T_free_var (Var ())

  let length substs =
    let length, _map = substs in
    length

  let subst ~to_ ~at_ substs =
    let length, map = substs in
    let map = Substs.add length (to_, at_) map in
    (1 + length, map)

  let expand_var var ~substs =
    match
      let length, map = substs in
      Substs.find_opt (length - 1 - var) map
    with
    | Some (to_, at_) when to_ == nil -> Either.Right at_
    | Some (to_, at_) ->
        let by_ = length substs - at_ in
        Either.Left (shift to_ ~depth:0 ~by_)
    | None -> failwith "malformed term"

  let rec normalize ~level term ~args ~substs =
    match (term, args) with
    (* subst *)
    | T_bound_var var, _ -> (
        match expand_var var ~substs with
        | Left term -> normalize ~level term ~args ~substs
        | Right var ->
            (* TODO: this is bad *)
            List.fold_left
              (fun lambda (arg, substs) ->
                let arg = normalize ~level arg ~args:[] ~substs in
                T_apply (lambda, arg))
              (T_bound_var (level - var))
              args)
    (* zeta *)
    | T_subst (body, value), _ ->
        let substs =
          let at_ = length substs in
          subst ~to_:value ~at_ substs
        in
        normalize ~level body ~args ~substs
    (* beta *)
    | T_lambda body, (arg, arg_substs) :: args ->
        let substs =
          let at_ = length arg_substs in
          subst ~to_:arg ~at_ substs
        in
        normalize ~level body ~args ~substs
    | T_apply (lambda, arg), _ ->
        let args = (arg, substs) :: args in
        normalize ~level lambda ~args ~substs
    (* head *)
    | T_free_var var, args ->
        List.fold_left
          (fun lambda (arg, substs) ->
            let arg = normalize ~level arg ~args:[] ~substs in
            T_apply (lambda, arg))
          (T_free_var var) args
    | T_lambda body, [] ->
        let body =
          let substs = subst ~to_:nil ~at_:level substs in
          let level = 1 + level in
          normalize ~level body ~args ~substs
        in
        T_lambda body

  let rec equal left right =
    match (left, right) with
    | T_subst _, _ | _, T_subst _ -> failwith "unreachable subst"
    | T_bound_var left, T_bound_var right when left == right -> ()
    | T_free_var left, T_free_var right when left == right -> ()
    | T_lambda left_body, T_lambda right_body -> equal left_body right_body
    | T_apply (left_lambda, left_arg), T_apply (right_lambda, right_arg) ->
        equal left_lambda right_lambda;
        equal left_arg right_arg
    | _ -> failwith "clash"

  let equal left ~left_substs right ~right_substs =
    let left = normalize ~level:0 left ~args:[] ~substs:left_substs in
    let right = normalize ~level:0 right ~args:[] ~substs:right_substs in
    equal left right
end

module Ln_but_closed = struct
  type index = int

  module Substs = Map.Make (Int)

  type term =
    (* M[...L] *)
    | T_closed of term * substs
    (* x *)
    | T_free_var of var
    (* \-n *)
    | T_bound_var of index
    (* λ. M *)
    | T_lambda of term
    (* M N *)
    | T_apply of term * term
    (* M[N] *)
    | T_subst of term * term

  and var = Var of unit
  and substs = { length : int; map : term Substs.t }

  let subst ~to_ substs =
    let { length; map } = substs in
    let map = Substs.add length to_ map in
    { length = 1 + length; map }

  let expand_var var ~substs =
    match
      let { length; map } = substs in
      Substs.find_opt (length - 1 - var) map
    with
    | Some to_ -> to_
    | None -> failwith "malformed term"

  let rec expand_head term ~substs =
    match term with
    (* subst *)
    | T_bound_var var ->
        let term = expand_var var ~substs in
        expand_head term ~substs
    (* zeta *)
    | T_closed (body, substs) -> expand_head body ~substs
    | T_subst (body, value) ->
        let value = T_closed (value, substs) in
        let substs = subst ~to_:value substs in
        expand_head body ~substs
    | T_apply (lambda, arg) -> (
        let arg = T_closed (arg, substs) in
        match expand_head lambda ~substs with
        | T_lambda body, substs ->
            (* beta *)
            let substs = subst ~to_:arg substs in
            expand_head body ~substs
        | lambda, substs ->
            (* head *)
            (T_apply (lambda, arg), substs))
    (* head *)
    | T_free_var _ | T_lambda _ -> (term, substs)

  let rec equal left ~left_substs right ~right_substs =
    let left, left_substs = expand_head left ~substs:left_substs in
    let right, right_substs = expand_head right ~substs:right_substs in
    equal_struct left ~left_substs right ~right_substs

  and equal_struct left ~left_substs right ~right_substs =
    match (left, right) with
    | T_bound_var _, _ | _, T_bound_var _ -> failwith "unreachable bound"
    | T_subst _, _ | _, T_subst _ -> failwith "unreachable subst"
    | T_free_var left, T_free_var right when left == right -> ()
    | T_lambda left_body, T_lambda right_body ->
        let skolem = T_free_var (Var ()) in
        let left_substs = subst ~to_:skolem left_substs in
        let right_substs = subst ~to_:skolem right_substs in
        equal left_body ~left_substs right_body ~right_substs
    | T_apply (left_lambda, left_arg), T_apply (right_lambda, right_arg) ->
        equal left_lambda ~left_substs right_lambda ~right_substs;
        equal left_arg ~left_substs right_arg ~right_substs
    | _ -> failwith "clash"

  let rec of_string env ~level term =
    let open Utils in
    let open Syntax.Ltree in
    let (LTerm { term; loc = _ }) = term in
    match term with
    | LT_var { var } -> (
        match Name.Map.find_opt var env with
        | Some at_ -> T_bound_var (level - at_)
        | None -> failwith @@ Format.asprintf "missing variable %a" Name.pp var)
    | LT_extension _ -> failwith "not supported"
    | LT_forall _ -> failwith "not supported"
    | LT_lambda { param; return } ->
        with_of_string_pat env ~level param @@ fun env ~level ->
        let return = of_string env ~level return in
        T_lambda return
    | LT_apply { lambda; arg } ->
        let lambda = of_string env ~level lambda in
        let arg = of_string env ~level arg in
        T_apply (lambda, arg)
    | LT_hoist _ -> failwith "not supported"
    | LT_let { bound; value; return } ->
        let value = of_string env ~level value in
        with_of_string_pat env ~level bound @@ fun env ~level ->
        let return = of_string env ~level return in
        T_subst (return, value)
    | LT_annot _ -> failwith "not supported"
    | LT_string _ -> failwith "not supported"

  and with_of_string_pat env ~level pat k =
    let open Utils in
    let open Syntax.Ltree in
    let (LPat { pat; loc = _ }) = pat in
    match pat with
    | LP_var { var } ->
        let level = 1 + level in
        let env = Name.Map.add var level env in
        k env ~level
    | LP_annot _ -> failwith "not supported"

  let of_string code =
    let open Utils in
    let open Syntax in
    let code =
      Lparser.parse_term @@ Option.get
      @@ Clexer.from_string Cparser.term_opt code
    in
    of_string Name.Map.empty ~level:0 code
end

module Ln_but_closed_cbv = struct
  type index = int

  module Substs = Map.Make (Int)

  type term = Ln_but_closed.term =
    (* M[...L] *)
    | T_closed of term * substs
    (* x *)
    | T_free_var of var
    (* \-n *)
    | T_bound_var of index
    (* λ. M *)
    | T_lambda of term
    (* M N *)
    | T_apply of term * term
    (* M[N] *)
    | T_subst of term * term

  and var = Ln_but_closed.var = Var of unit
  and substs = Ln_but_closed.substs = { length : int; map : term Substs.t }

  let subst ~to_ substs =
    let { length; map } = substs in
    let map = Substs.add length to_ map in
    { length = 1 + length; map }

  let expand_var var ~substs =
    match
      let { length; map } = substs in
      Substs.find_opt (length - 1 - var) map
    with
    | Some to_ -> to_
    | None -> failwith "malformed term"

  let rec expand_head term ~substs =
    match term with
    | T_closed (body, substs) ->
        (* closed *)
        expand_head body ~substs
    | T_bound_var var ->
        (* subst *)
        let term = expand_var var ~substs in
        expand_head term ~substs
    | T_subst (body, value) ->
        (* zeta *)
        let to_ =
          let to_, to_substs = expand_head value ~substs in
          (* TODO: ideally this would not be closed? *)
          T_closed (to_, to_substs)
        in
        let substs = subst ~to_ substs in
        expand_head body ~substs
    | T_apply (lambda, arg) -> (
        match expand_head lambda ~substs with
        | T_lambda body, lambda_substs ->
            (* beta *)
            let to_ =
              let arg, arg_substs = expand_head arg ~substs in
              T_closed (arg, arg_substs)
            in
            let substs = subst ~to_ lambda_substs in
            expand_head body ~substs
        | lambda, lambda_substs ->
            (* head apply *)
            let arg = T_closed (arg, substs) in
            (T_apply (lambda, arg), lambda_substs))
    | T_free_var _ ->
        (* head var *)
        (term, substs)
    | T_lambda _ ->
        (* head lambda*)
        (term, substs)

  let rec equal left ~left_substs right ~right_substs =
    let left, left_substs = expand_head left ~substs:left_substs in
    let right, right_substs = expand_head right ~substs:right_substs in
    equal_struct left ~left_substs right ~right_substs

  and equal_struct left ~left_substs right ~right_substs =
    match (left, right) with
    | T_bound_var _, _ | _, T_bound_var _ -> failwith "unreachable bound"
    | T_subst _, _ | _, T_subst _ -> failwith "unreachable subst"
    | T_free_var left, T_free_var right when left == right -> ()
    | T_lambda left_body, T_lambda right_body ->
        let skolem = T_free_var (Var ()) in
        let left_substs = subst ~to_:skolem left_substs in
        let right_substs = subst ~to_:skolem right_substs in
        equal left_body ~left_substs right_body ~right_substs
    | T_apply (left_lambda, left_arg), T_apply (right_lambda, right_arg) ->
        equal left_lambda ~left_substs right_lambda ~right_substs;
        equal left_arg ~left_substs right_arg ~right_substs
    | _ -> failwith "clash"
end

(* module Ln_but_stacks_with_shifts_in_stack = struct
     type index = int

     type term = Locally_nameless.term =
       (* \n *)
       | T_free_var of var
       (* \-n *)
       | T_bound_var of index
       (* λ. M *)
       | T_lambda of term
       (* M N *)
       | T_apply of term * term
       (* M[N] *)
       | T_subst of term * term

     and var = Locally_nameless.var = Var of unit

     type shifts = Nil | Shift of { by_ : int; at_ : int; next : shifts }

     let shift ~from ~length ~shifts =
       let by_ = length - from in
       Shift { by_; at_ = length; next = shifts }

     let max_length = 1024 * 1024

     module Shifts : sig
       type shifts
       type t = shifts
       type ptr

       val make : unit -> shifts

       (* branch *)
       val nil : ptr
       val fork : shifts -> ptr
       val load : shifts -> ptr -> unit

       (* data *)
       val shift : shifts -> by_:int -> at_:int -> unit
       val apply : shifts -> substs:int -> var:int -> int
     end = struct
       type shifts = {
         data : int array;
         mutable next : int;
         mutable length : int;
         mutable offset : int;
       }

       type t = shifts
       type ptr = int

       let make () =
         let data = Array.make max_length 0 in
         { data; next = 0; length = 0; offset = 0 }

       let nil = 0

       let fork shifts =
         let { data; next; length; offset } = shifts in
         (* bump allocate *)
         shifts.offset <- 2 + offset;
         (* write header *)
         Array.set data offset next;
         Array.set data (1 + offset) length;
         (* point to previous *)
         shifts.next <- offset;
         shifts.length <- 0;
         (* header pointer *)
         offset

       let load shifts ptr =
         (* TODO: drop non forked data *)
         shifts.next <- ptr;
         shifts.length <- 0

       let shift shifts ~by_ ~at_ =
         let { next = _; data; length; offset } = shifts in
         (* write data *)
         Array.set data offset by_;
         Array.set data (1 + offset) at_;
         (* bump allocate *)
         shifts.length <- 1 + length;
         shifts.offset <- 2 + offset

       let rec apply ~data ~substs var ~next ~length ~offset =
         match length with
         | 0 -> (
             match next = nil with
             | true -> var
             | false ->
                 let offset = next in
                 let next = Array.get data offset in
                 let length = Array.get data (1 + offset) in
                 apply ~data ~substs var ~next ~length ~offset)
         | _ -> (
             let by_ = Array.get data offset in
             let at_ = Array.get data (1 + offset) in
             (* TODO: should this substs decreases? *)
             let distance = substs - at_ in
             match var >= distance with
             | true ->
                 let length = length - 1 in
                 let offset = offset - 2 in
                 let var = by_ + var in
                 apply ~data ~substs var ~next ~length ~offset
             | false -> var)

       let apply shifts ~substs ~var =
         let { data; next; length; offset } = shifts in
         apply ~data ~substs var ~next ~length ~offset
     end

     module Substs : sig
       type substs

       val make : unit -> substs
       val clear : substs -> unit
       val length : substs -> int

       (* *)
       val shifts : substs -> shifts
       val load_shifts : substs -> shifts -> unit

       (* *)
       val apply : substs -> var:int -> term
       val push : substs -> term -> int -> shifts -> unit
     end = struct
       type substs = {
         mutable length : int;
         mutable shifts : shifts;
         terms : term array;
         positions : int array;
         terms_shifts : shifts array;
       }

       let nil = T_free_var (Var ())

       let make () =
         let terms = Array.make max_length nil in
         let positions = Array.make max_length 0 in
         let terms_shifts = Array.make max_length Nil in
         { length = 0; shifts = Nil; terms; positions; terms_shifts }

       let clear substs =
         substs.length <- 0;
         substs.shifts <- Nil

       let length substs = substs.length
       let shifts substs = substs.shifts
       let load_shifts substs shifts = substs.shifts <- shifts

       let push substs value at_ at_shifts =
         let { length; shifts = _; terms; positions; terms_shifts } = substs in
         Array.set terms length value;
         Array.set positions length at_;
         Array.set terms_shifts length at_shifts;
         substs.length <- 1 + length

       let rec apply var ~length ~shifts =
         match shifts with
         | Nil -> var
         | Shift { by_; at_; next = shifts } -> (
             let distance =
               (* TODO: should this length decreases? *)
               length - at_
             in
             match var >= distance with
             | true ->
                 let var = by_ + var in
                 apply var ~length ~shifts
             | false -> var)

       let apply substs ~var =
         let { length; shifts; terms; positions; terms_shifts } = substs in
         let offset = apply var ~length ~shifts in
         let index = length - 1 - offset in
         let term = Array.get terms index in
         let from = Array.get positions index in
         let () =
           let shifts = Array.get terms_shifts index in
           substs.shifts <- shift ~from ~length ~shifts
         in
         term
     end

     let rec expand_head term ~args ~substs =
       match (term, args) with
       (* subst *)
       | T_bound_var var, _ ->
           let term = Substs.apply substs ~var in
           expand_head term ~args ~substs
       (* zeta *)
       | T_subst (body, value), _ ->
           let () =
             let length = Substs.length substs in
             let shifts = Substs.shifts substs in
             Substs.push substs value length shifts
           in
           expand_head body ~args ~substs
       (* beta *)
       | T_lambda body, (arg, at_, arg_shifts) :: args ->
           let () = Substs.push substs arg at_ arg_shifts in
           expand_head body ~args ~substs
       | T_apply (lambda, arg), _ ->
           let args =
             let length = Substs.length substs in
             let shifts = Substs.shifts substs in
             (arg, length, shifts) :: args
           in
           expand_head lambda ~args ~substs
       (* head *)
       | T_free_var _, _ | T_lambda _, [] -> (term, args, substs)

     let rec equal left ~left_substs right ~right_substs =
       let left, left_args, left_substs =
         expand_head left ~args:[] ~substs:left_substs
       in
       let right, right_args, right_substs =
         expand_head right ~args:[] ~substs:right_substs
       in
       equal_struct left ~left_substs right ~right_substs;
       (* TODO: clash *)
       List.iter2
         (fun (left, left_from, left_shifts) (right, right_from, right_shifts) ->
           let left_shifts =
             let length = Substs.length left_substs in
             shift ~from:left_from ~length ~shifts:left_shifts
           in
           let () = Substs.load_shifts left_substs left_shifts in
           let right_shifts =
             let length = Substs.length right_substs in
             shift ~from:right_from ~length ~shifts:right_shifts
           in
           let () = Substs.load_shifts right_substs right_shifts in
           equal left ~left_substs right ~right_substs)
         left_args right_args

     and equal_struct left ~left_substs right ~right_substs =
       match (left, right) with
       | T_bound_var _, _ | _, T_bound_var _ -> failwith "unreachable bound"
       | T_apply (_, _), _ | _, T_apply (_, _) -> failwith "unreachable apply"
       | T_subst _, _ | _, T_subst _ -> failwith "unreachable subst"
       | T_free_var left, T_free_var right when left == right -> ()
       | T_lambda left_body, T_lambda right_body ->
           let skolem = T_free_var (Var ()) in
           let () =
             let length = Substs.length left_substs in
             Substs.push left_substs skolem length Nil
           in
           let () =
             let length = Substs.length right_substs in
             Substs.push right_substs skolem length Nil
           in
           equal left_body ~left_substs right_body ~right_substs
       | _ -> failwith "clash"
   end *)

module Substs_var = struct
  type level = int
  type index = int

  type term =
    (* x *)
    | T_context_var of var
    (* \+n *)
    | T_substs_var of level
    (* \-n *)
    | T_term_var of index
    (* λ. M *)
    | T_lambda of term
    (* M N *)
    | T_apply of term * term
    (* M[N] *)
    | T_subst of term * term

  and var = Var of unit

  let rec pack ~simple term ~depth ~at_ =
    let pack term ~depth ~at_ = pack ~simple term ~depth ~at_ in
    match term with
    | T_context_var var -> T_context_var var
    | T_substs_var var -> (
        match var >= at_ with
        | true ->
            (* TODO: this never happens *)
            assert false
        | false -> T_substs_var var)
    | T_term_var var -> (
        match var >= depth with
        | true ->
            let top = at_ - 1 in
            T_substs_var (top + depth - var)
        | false -> T_term_var var)
    | T_lambda body ->
        simple := false;
        let body = pack body ~depth:(1 + depth) ~at_ in
        T_lambda body
    | T_apply (lambda, arg) ->
        let lambda = pack lambda ~depth ~at_ in
        let arg = pack arg ~depth ~at_ in
        T_apply (lambda, arg)
    | T_subst (body, value) ->
        let value = pack value ~depth ~at_ in
        let body = pack body ~depth:(1 + depth) ~at_ in
        T_subst (body, value)

  let pack term ~depth ~at_ =
    let simple = ref true in
    pack ~simple term ~depth ~at_

  module Substs = Map.Make (Int)

  let length substs =
    match Substs.max_binding_opt substs with
    | Some (_, (_to, length)) -> length
    | None -> 0

  let subst ~to_ substs =
    let length = length substs in
    Substs.add length (to_, 1 + length) substs

  let expand_term_var var ~substs =
    let length = length substs in
    match Substs.find_opt (length - 1 - var) substs with
    | Some (to_, _length) -> to_
    | None -> failwith "malformed term"

  let expand_substs_var var ~substs =
    let length = length substs in
    expand_term_var (length - 1 - var) ~substs

  let rec expand_head term ~args ~substs k =
    match (term, args) with
    (* subst *)
    | T_substs_var var, _ ->
        let term = expand_substs_var var ~substs in
        expand_head term ~args ~substs k
    | T_term_var var, _ ->
        let term = expand_term_var var ~substs in
        expand_head term ~args ~substs k
    (* zeta *)
    | T_subst (body, value), _ ->
        let substs =
          let at_ = length substs in
          let value = pack value ~depth:0 ~at_ in
          subst ~to_:value substs
        in
        expand_head body ~args ~substs k
    (* beta *)
    | T_lambda body, (arg, arg_substs) :: args ->
        let substs =
          let at_ = length arg_substs in
          let arg = pack arg ~depth:0 ~at_ in
          subst ~to_:arg substs
        in
        expand_head body ~args ~substs k
    | T_apply (lambda, arg), _ ->
        let args = (arg, substs) :: args in
        expand_head lambda ~args ~substs k
    (* head *)
    | T_context_var _, _ | T_lambda _, [] -> k term ~args ~substs

  let rec equal left ~left_substs right ~right_substs =
    expand_head left ~args:[] ~substs:left_substs
    @@ fun left ~args:left_args ~substs:left_substs ->
    expand_head right ~args:[] ~substs:right_substs
    @@ fun right ~args:right_args ~substs:right_substs ->
    equal_struct left ~left_substs right ~right_substs;
    (* TODO: clash *)
    List.iter2
      (fun (left, left_substs) (right, right_substs) ->
        equal left ~left_substs right ~right_substs)
      left_args right_args

  and equal_struct left ~left_substs right ~right_substs =
    match (left, right) with
    | T_substs_var _, _ | _, T_substs_var _ ->
        failwith "unreachable substs bound var"
    | T_term_var _, _ | _, T_term_var _ -> failwith "unreachable term bound var"
    | T_apply (_, _), _ | _, T_apply (_, _) -> failwith "unreachable apply"
    | T_subst _, _ | _, T_subst _ -> failwith "unreachable subst"
    | T_context_var left, T_context_var right when left == right -> ()
    | T_lambda left_body, T_lambda right_body ->
        let skolem = T_context_var (Var ()) in
        let left_substs = subst ~to_:skolem left_substs in
        let right_substs = subst ~to_:skolem right_substs in
        equal left_body ~left_substs right_body ~right_substs
    | _ -> failwith "clash"

  let rec of_string env ~level term =
    let open Utils in
    let open Syntax.Ltree in
    let (LTerm { term; loc = _ }) = term in
    match term with
    | LT_var { var } -> (
        match Name.Map.find_opt var env with
        | Some at_ -> T_term_var (level - at_)
        | None -> failwith @@ Format.asprintf "missing variable %a" Name.pp var)
    | LT_extension _ -> failwith "not supported"
    | LT_forall _ -> failwith "not supported"
    | LT_lambda { param; return } ->
        with_of_string_pat env ~level param @@ fun env ~level ->
        let return = of_string env ~level return in
        T_lambda return
    | LT_apply { lambda; arg } ->
        let lambda = of_string env ~level lambda in
        let arg = of_string env ~level arg in
        T_apply (lambda, arg)
    | LT_hoist _ -> failwith "not supported"
    | LT_let { bound; value; return } ->
        let value = of_string env ~level value in
        with_of_string_pat env ~level bound @@ fun env ~level ->
        let return = of_string env ~level return in
        T_subst (return, value)
    | LT_annot _ -> failwith "not supported"
    | LT_string _ -> failwith "not supported"

  and with_of_string_pat env ~level pat k =
    let open Utils in
    let open Syntax.Ltree in
    let (LPat { pat; loc = _ }) = pat in
    match pat with
    | LP_var { var } ->
        let level = 1 + level in
        let env = Name.Map.add var level env in
        k env ~level
    | LP_annot _ -> failwith "not supported"

  let of_string code =
    let open Utils in
    let open Syntax in
    let code =
      Lparser.parse_term @@ Option.get
      @@ Clexer.from_string Cparser.term_opt code
    in
    of_string Name.Map.empty ~level:0 code
end

module Substs_var_but_stack = struct
  type level = int
  type index = int

  type term = Substs_var.term =
    (* x *)
    | T_context_var of var
    (* \+n *)
    | T_substs_var of level
    (* \-n *)
    | T_term_var of index
    (* λ. M *)
    | T_lambda of term
    (* M N *)
    | T_apply of term * term
    (* M[N] *)
    | T_subst of term * term

  and var = Substs_var.var = Var of unit

  let rec pack term ~depth ~at_ =
    let pack term ~depth ~at_ = pack term ~depth ~at_ in
    match term with
    | T_context_var var -> T_context_var var
    | T_substs_var var -> (
        match var >= at_ with
        | true ->
            (* TODO: this never happens *)
            assert false
        | false -> T_substs_var var)
    | T_term_var var -> (
        match var >= depth with
        | true ->
            let top = at_ - 1 in
            T_substs_var (top + depth - var)
        | false -> T_term_var var)
    | T_lambda body ->
        let body = pack body ~depth:(1 + depth) ~at_ in
        T_lambda body
    | T_apply (lambda, arg) ->
        let lambda = pack lambda ~depth ~at_ in
        let arg = pack arg ~depth ~at_ in
        T_apply (lambda, arg)
    | T_subst (body, value) ->
        let value = pack value ~depth ~at_ in
        let body = pack body ~depth:(1 + depth) ~at_ in
        T_subst (body, value)

  module Substs : sig
    type substs

    val make : unit -> substs
    val last : length:int -> substs -> int -> term
    val find : substs -> int -> term
    val push : length:int -> substs -> term -> int
  end = struct
    type substs = term array

    let nil = T_context_var (Var ())

    let make () =
      let max_length = 512 * 1024 in
      Array.make max_length nil

    let push ~length substs value =
      Array.set substs length value;
      1 + length

    let find substs index = Array.get substs index

    let last ~length substs offset =
      let index = length - 1 - offset in
      Array.get substs index
  end

  let rec expand_head term ~args ~length ~substs =
    match (term, args) with
    (* subst *)
    | T_substs_var var, _ ->
        let term = Substs.find substs var in
        expand_head term ~args ~length ~substs
    | T_term_var var, _ ->
        let term = Substs.last ~length substs var in
        expand_head term ~args ~length ~substs
    (* zeta *)
    | T_subst (body, value), _ ->
        let value = pack value ~depth:0 ~at_:length in
        let length = Substs.push ~length substs value in
        expand_head body ~args ~length ~substs
    (* beta *)
    | T_lambda body, arg :: args ->
        let length = Substs.push ~length substs arg in
        expand_head body ~args ~length ~substs
    | T_apply (lambda, arg), _ ->
        let arg = pack arg ~depth:0 ~at_:length in
        let args = arg :: args in
        expand_head lambda ~args ~length ~substs
    (* head *)
    | T_context_var _, _ | T_lambda _, [] -> (term, args, length, substs)

  let rec equal left ~left_length ~left_substs right ~right_length ~right_substs
      =
    let left, left_args, left_length, left_substs =
      expand_head left ~args:[] ~length:left_length ~substs:left_substs
    in
    let right, right_args, right_length, right_substs =
      expand_head right ~args:[] ~length:right_length ~substs:right_substs
    in
    equal_struct left ~left_length ~left_substs right ~right_length
      ~right_substs;
    (* TODO: clash *)
    List.iter2
      (fun left right ->
        equal left ~left_length ~left_substs right ~right_length ~right_substs)
      left_args right_args

  and equal_struct left ~left_length ~left_substs right ~right_length
      ~right_substs =
    match (left, right) with
    | T_substs_var _, _ | _, T_substs_var _ ->
        failwith "unreachable substs bound var"
    | T_term_var _, _ | _, T_term_var _ -> failwith "unreachable term bound var"
    | T_apply (_, _), _ | _, T_apply (_, _) -> failwith "unreachable apply"
    | T_subst _, _ | _, T_subst _ -> failwith "unreachable subst"
    | T_context_var left, T_context_var right when left == right -> ()
    | T_lambda left_body, T_lambda right_body ->
        let skolem = T_context_var (Var ()) in
        let left_length = Substs.push ~length:left_length left_substs skolem in
        let right_length =
          Substs.push ~length:right_length right_substs skolem
        in
        equal left_body ~left_length ~left_substs right_body ~right_length
          ~right_substs
    | _ -> failwith "clash"
end

module Lazy_substs_var = struct
  type level = int
  type index = int

  type term =
    (* x *)
    | T_context_var of var
    (* \+n *)
    | T_substs_var of level
    (* \-n *)
    | T_term_var of index
    (* λ. M *)
    | T_lambda of term
    (* M N *)
    | T_apply of term * term
    (* M[N] *)
    | T_subst of term * term

  and var = Var of unit

  let rec pack term ~depth ~at_ =
    match term with
    | T_context_var var -> T_context_var var
    | T_substs_var var -> (
        match var >= at_ with
        | true ->
            let top = at_ - 1 in
            T_term_var (var + depth - top)
        | false -> T_substs_var var)
    | T_term_var var -> (
        match var >= depth with
        | true ->
            let top = at_ - 1 in
            T_substs_var (top + depth - var)
        | false -> T_term_var var)
    | T_lambda body ->
        let body = pack body ~depth:(1 + depth) ~at_ in
        T_lambda body
    | T_apply (lambda, arg) ->
        let lambda = pack lambda ~depth ~at_ in
        let arg = pack arg ~depth ~at_ in
        T_apply (lambda, arg)
    | T_subst (body, value) ->
        let value = pack value ~depth ~at_ in
        let body = pack body ~depth:(1 + depth) ~at_ in
        T_subst (body, value)

  module Substs = Map.Make (Int)

  let length substs =
    match Substs.max_binding_opt substs with
    | Some (_, (_to, length)) -> length
    | None -> 0

  let subst ~to_ substs =
    let length = length substs in
    Substs.add length (to_, 1 + length) substs

  let expand_term_var var ~substs =
    let length = length substs in
    match Substs.find_opt (length - 1 - var) substs with
    | Some (to_, _length) -> to_
    | None -> failwith "malformed term"

  let expand_substs_var var ~substs =
    let length = length substs in
    expand_term_var (length - 1 - var) ~substs

  let rec expand_head term ~args ~substs k =
    match (term, args) with
    (* subst *)
    | T_substs_var var, _ ->
        let term = Lazy.force @@ expand_substs_var var ~substs in
        expand_head term ~args ~substs k
    | T_term_var var, _ ->
        let term = Lazy.force @@ expand_term_var var ~substs in
        expand_head term ~args ~substs k
    (* zeta *)
    | T_subst (body, value), _ ->
        let substs =
          let at_ = length substs in
          let value = lazy (pack value ~depth:0 ~at_) in
          subst ~to_:value substs
        in
        expand_head body ~args ~substs k
    (* beta *)
    | T_lambda body, (arg, arg_substs) :: args ->
        let substs =
          let at_ = length arg_substs in
          let arg = lazy (pack arg ~depth:0 ~at_) in
          subst ~to_:arg substs
        in
        expand_head body ~args ~substs k
    | T_apply (lambda, arg), _ ->
        let args = (arg, substs) :: args in
        expand_head lambda ~args ~substs k
    (* head *)
    | T_context_var _, _ | T_lambda _, [] -> k term ~args ~substs

  let rec equal left ~left_substs right ~right_substs =
    expand_head left ~args:[] ~substs:left_substs
    @@ fun left ~args:left_args ~substs:left_substs ->
    expand_head right ~args:[] ~substs:right_substs
    @@ fun right ~args:right_args ~substs:right_substs ->
    equal_struct left ~left_substs right ~right_substs;
    (* TODO: clash *)
    List.iter2
      (fun (left, left_substs) (right, right_substs) ->
        equal left ~left_substs right ~right_substs)
      left_args right_args

  and equal_struct left ~left_substs right ~right_substs =
    match (left, right) with
    | T_substs_var _, _ | _, T_substs_var _ ->
        failwith "unreachable substs bound var"
    | T_term_var _, _ | _, T_term_var _ -> failwith "unreachable term bound var"
    | T_apply (_, _), _ | _, T_apply (_, _) -> failwith "unreachable apply"
    | T_subst _, _ | _, T_subst _ -> failwith "unreachable subst"
    | T_context_var left, T_context_var right when left == right -> ()
    | T_lambda left_body, T_lambda right_body ->
        let skolem = lazy (T_context_var (Var ())) in
        let left_substs = subst ~to_:skolem left_substs in
        let right_substs = subst ~to_:skolem right_substs in
        equal left_body ~left_substs right_body ~right_substs
    | _ -> failwith "clash"

  let rec of_string env ~level term =
    let open Utils in
    let open Syntax.Ltree in
    let (LTerm { term; loc = _ }) = term in
    match term with
    | LT_var { var } -> (
        match Name.Map.find_opt var env with
        | Some at_ -> T_term_var (level - at_)
        | None -> failwith @@ Format.asprintf "missing variable %a" Name.pp var)
    | LT_extension _ -> failwith "not supported"
    | LT_forall _ -> failwith "not supported"
    | LT_lambda { param; return } ->
        with_of_string_pat env ~level param @@ fun env ~level ->
        let return = of_string env ~level return in
        T_lambda return
    | LT_apply { lambda; arg } ->
        let lambda = of_string env ~level lambda in
        let arg = of_string env ~level arg in
        T_apply (lambda, arg)
    | LT_hoist _ -> failwith "not supported"
    | LT_let { bound; value; return } ->
        let value = of_string env ~level value in
        with_of_string_pat env ~level bound @@ fun env ~level ->
        let return = of_string env ~level return in
        T_subst (return, value)
    | LT_annot _ -> failwith "not supported"
    | LT_string _ -> failwith "not supported"

  and with_of_string_pat env ~level pat k =
    let open Utils in
    let open Syntax.Ltree in
    let (LPat { pat; loc = _ }) = pat in
    match pat with
    | LP_var { var } ->
        let level = 1 + level in
        let env = Name.Map.add var level env in
        k env ~level
    | LP_annot _ -> failwith "not supported"

  let of_string code =
    let open Utils in
    let open Syntax in
    let code =
      Lparser.parse_term @@ Option.get
      @@ Clexer.from_string Cparser.term_opt code
    in
    of_string Name.Map.empty ~level:0 code
end

module Just_another_machine_bro = struct
  type index = int

  type term =
    (* \n *)
    | T_bound_var of index
    (* λ. M *)
    | T_lambda of term
    (* M N *)
    | T_apply of term * term
    (* M[N] *)
    | T_subst of term * term
end
(* module Machine_like = struct
     type term =
       | BOUND_VAR of { var : int }
       | APPLY of { funct_length : int }
       | LAMBDA of { body_length : int }
       | LET of { value_length : int }

     let incr x = 1 + x
     let rec expand_head instr substs stack = assert false
   end *)

let n256 =
  {|z => s => 
      s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
      s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
      s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
      s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
      s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
      s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
      s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
      s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
      s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
      s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
      s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
      s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
      s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
      s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
      s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
      s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
      z
      ))))))))))))))))
      ))))))))))))))))
      ))))))))))))))))
      ))))))))))))))))
      ))))))))))))))))
      ))))))))))))))))
      ))))))))))))))))
      ))))))))))))))))
      ))))))))))))))))
      ))))))))))))))))
      ))))))))))))))))
      ))))))))))))))))
      ))))))))))))))))
      ))))))))))))))))
      ))))))))))))))))
      ))))))))))))))))
|}

let small_code_n256 =
  {|
    zero = z => s => z;
    succ = pred => z => s => s (pred z s);
  
    one = succ zero;
  
    add = a => b => a b succ;
    mul = a => b => a zero (n => add n b);
  
    two = succ one;
    n4 = mul two two;
    n8 = mul n4 two;
    n16 = mul n8 two;
    n32 = mul n16 two;
    n64 = mul n32 two;
    n128 = mul n64 two;
    n256 = mul n128 two;
    n256
  |}

let small_code_n512 =
  {|
    zero = z => s => z;
    succ = pred => z => s => s (pred z s);

    one = succ zero;

    add = a => b => a b succ;
    mul = a => b => a zero (n => add n b);

    two = succ one;
    n4 = mul two two;
    n8 = mul n4 two;
    n16 = mul n8 two;
    n32 = mul n16 two;
    n64 = mul n32 two;
    n128 = mul n64 two;
    n256 = mul n128 two;
    n512 = mul n256 two;
    n512
  |}

let n512 =
  {|z => s => 
      s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
      s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
      s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
      s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
      s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
      s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
      s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
      s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
      s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
      s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
      s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
      s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
      s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
      s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
      s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
      s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
      s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
      s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
      s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
      s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
      s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
      s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
      s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
      s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
      s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
      s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
      s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
      s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
      s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
      s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
      s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
      s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(
      z
      ))))))))))))))))
      ))))))))))))))))
      ))))))))))))))))
      ))))))))))))))))
      ))))))))))))))))
      ))))))))))))))))
      ))))))))))))))))
      ))))))))))))))))
      ))))))))))))))))
      ))))))))))))))))
      ))))))))))))))))
      ))))))))))))))))
      ))))))))))))))))
      ))))))))))))))))
      ))))))))))))))))
      ))))))))))))))))
      ))))))))))))))))
      ))))))))))))))))
      ))))))))))))))))
      ))))))))))))))))
      ))))))))))))))))
      ))))))))))))))))
      ))))))))))))))))
      ))))))))))))))))
      ))))))))))))))))
      ))))))))))))))))
      ))))))))))))))))
      ))))))))))))))))
      ))))))))))))))))
      ))))))))))))))))
      ))))))))))))))))
      ))))))))))))))))
|}

let[@inline.always] bench name f =
  let deltas =
    List.init 3 (fun _ ->
        let t1 = Unix.gettimeofday () in
        f ();
        let t2 = Unix.gettimeofday () in
        t2 -. t1)
  in
  let total = List.fold_left (fun a b -> a +. b) 0.0 deltas in
  let average = total /. (Int.to_float @@ List.length deltas) in
  Format.eprintf "%s: %.3f\n%!" name average

let tests =
  [
    ("static_n256", n256, n256);
    ("compute_n256", small_code_n256, n256);
    ("static_n512", n512, n512);
    ("compute_n512", small_code_n512, n512);
  ]

let second () =
  let () =
    let open Locally_nameless in
    List.iter
      (fun (name, left, right) ->
        let left = of_string left in
        let right = of_string right in
        bench (Format.asprintf "ln_%s" name) @@ fun () ->
        equal left ~left_substs:(0, Substs.empty) right
          ~right_substs:(0, Substs.empty))
      tests
  in
  let () =
    let open Ln_but_normal in
    List.iter
      (fun (name, left, right) ->
        let left = Locally_nameless.of_string left in
        let right = Locally_nameless.of_string right in
        bench (Format.asprintf "ln_but_normal_%s" name) @@ fun () ->
        equal left ~left_substs:(0, Substs.empty) right
          ~right_substs:(0, Substs.empty))
      tests
  in

  let () =
    let open Substs_var in
    List.iter
      (fun (name, left, right) ->
        let left = of_string left in
        let right = of_string right in
        bench (Format.asprintf "substs_var_%s" name) @@ fun () ->
        equal left ~left_substs:Substs.empty right ~right_substs:Substs.empty)
      tests
  in
  let () =
    let open Lazy_substs_var in
    List.iter
      (fun (name, left, right) ->
        let left = of_string left in
        let right = of_string right in
        bench (Format.asprintf "lazy_substs_var_%s" name) @@ fun () ->
        equal left ~left_substs:Substs.empty right ~right_substs:Substs.empty)
      tests
  in
  let () = Gc.compact () in
  let () =
    let open Ln_but_stacks_with_bounded_shifts in
    let left_substs = Substs.make () in
    let right_substs = Substs.make () in
    List.iter
      (fun (name, left, right) ->
        let left = Locally_nameless.of_string left in
        let right = Locally_nameless.of_string right in
        bench (Format.asprintf "ln_but_stack_with_bounded_shifts_%s" name)
        @@ fun () ->
        equal left ~left_length:0 ~left_shifts:Nil ~left_substs right
          ~right_length:0 ~right_shifts:Nil ~right_substs)
      tests
  in

  Format.eprintf "done"

let main () =
  let () = Gc.compact () in
  let () =
    let open Ln_but_closed in
    List.iter
      (fun (name, left, right) ->
        let left = of_string left in
        let right = of_string right in
        bench (Format.asprintf "ln_but_closed_%s" name) @@ fun () ->
        equal left
          ~left_substs:{ length = 0; map = Substs.empty }
          right
          ~right_substs:{ length = 0; map = Substs.empty })
      tests
  in
  let () = Gc.compact () in
  let () =
    let open Ln_but_closed_cbv in
    List.iter
      (fun (name, left, right) ->
        let left = Ln_but_closed.of_string left in
        let right = Ln_but_closed.of_string right in
        bench (Format.asprintf "ln_but_closed_cbv_%s" name) @@ fun () ->
        equal left
          ~left_substs:{ length = 0; map = Substs.empty }
          right
          ~right_substs:{ length = 0; map = Substs.empty })
      tests
  in
  let () = Gc.compact () in
  let () =
    let open Ln_but_stacks in
    let left_substs = Substs.make () in
    let right_substs = Substs.make () in
    List.iter
      (fun (name, left, right) ->
        let left = Locally_nameless.of_string left in
        let right = Locally_nameless.of_string right in
        bench (Format.asprintf "ln_but_stack_%s" name) @@ fun () ->
        equal left ~left_length:0 ~left_substs right ~right_length:0
          ~right_substs)
      tests
  in
  let () = Gc.compact () in
  let () =
    let open Ln_but_stacks_with_shifts in
    let left_substs = Substs.make () in
    let right_substs = Substs.make () in
    List.iter
      (fun (name, left, right) ->
        let left = Locally_nameless.of_string left in
        let right = Locally_nameless.of_string right in
        bench (Format.asprintf "ln_but_stack_with_shifts_%s" name) @@ fun () ->
        equal left ~left_length:0 ~left_shifts:Nil ~left_substs right
          ~right_length:0 ~right_shifts:Nil ~right_substs)
      tests
  in
  let () = Gc.compact () in
  let () =
    let open Ln_but_stacks_with_shifts_packed in
    let left_substs = Substs.make () in
    let right_substs = Substs.make () in
    List.iter
      (fun (name, left, right) ->
        Substs.clear left_substs;
        Substs.clear right_substs;
        let left = Locally_nameless.of_string left in
        let right = Locally_nameless.of_string right in
        bench (Format.asprintf "ln_but_stack_with_shifts_packed_%s" name)
        @@ fun () -> equal left ~left_substs right ~right_substs)
      tests
  in

  let () = Gc.compact () in
  let () =
    let open Substs_var_but_stack in
    let left_substs = Substs.make () in
    let right_substs = Substs.make () in
    List.iter
      (fun (name, left, right) ->
        let left = Substs_var.of_string left in
        let right = Substs_var.of_string right in
        bench (Format.asprintf "substs_var_but_stack_%s" name) @@ fun () ->
        equal left ~left_length:0 ~left_substs right ~right_length:0
          ~right_substs)
      tests
  in

  (*
     let () = Gc.compact () in
     let () =
          let open Ln_but_stacks_with_shifts_in_stack in
          let left_substs = Substs.make () in
          let right_substs = Substs.make () in
          List.iter
            (fun (name, left, right) ->
              let left = Locally_nameless.of_string left in
              let right = Locally_nameless.of_string right in
              bench (Format.asprintf "ln_but_stack_with_shifts_in_stack_%s" name)
              @@ fun () ->
              equal left ~left_length:0 ~left_shifts:Nil ~left_substs right
                ~right_length:0 ~right_shifts:Nil ~right_substs)
            tests
        in
  *)
  ()

let _ = second
let () = main ()
