module rec Level : sig
  type level
  type t = level

  val ( <= ) : level -> level -> bool
  val zero : level
  val min : level -> level -> level
  val max : level -> level -> level
end =
  Level

module Graph = struct
  type term =
    | Term of {
        mutable link : term;
        head : term_head;
        mutable usages : term list;
      }

  and term_head =
    | T_var of { var : var }
    | T_lambda of { bound : var; body : term }
    | T_apply of { funct : term; arg : term }

  and var = Var of { mutable instances : term list }

  let is_linear : term -> bool = _
  let is_affine_funct : var -> term -> bool = _
  let t_head : term -> term_head = _

  let rec optimize term =
    match t_head term with
    | T_var { var } -> (
        let to_ = _ in
        match is_constant to_ with true -> t_link term ~to_ | false -> _)
    | T_lambda { bound; body } -> _
    | T_apply { funct; arg } -> (
        match t_head funct with
        | T_var _ -> _
        | T_lambda { bound; body } -> (
            match is_linear funct || is_affine_funct bound body with
            | true -> _
            | false -> _)
        | T_apply _ -> _)
end

module Typed_tree = struct
  type term =
    | T_var of { var : var }
    | T_apply of { funct : term; arg : term }
    | T_lambda of { var : var; body : term }
    | T_type
    | T_pi of { param : term; body : term }
    | T_self of { body : term }

  and var
end

module Unboxed_tree = struct
  type term =
    | T_var of { var : var }
    | TF_type
    | TE_len of { type_ : term }
    | TF_self of { body : term }
    | TI_fix of { body : term }
    | TE_unfold of { fix : term }
    | TF_pi of { param : term; body : term }
    | TI_lambda of { var : var; body : term }
    | TE_apply of { funct : term; arg : term }
    | TF_sigma of { param : term; body : term }
    | TI_pair of { fst : term; snd : term }
    | TE_fst of { pair : term }
    | TE_snd of { pair : term }
    | TF_box of { content : term }
    | TI_alloc of { content : term }
    | TE_deref of { box : term }

  and var
end

module Lower = struct end
