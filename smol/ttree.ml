type loc = Location
type typed = Typed
type core = Core

type _ term =
  | TT_loc : { term : _ term; loc : Location.t } -> loc term
  | TT_typed : { term : _ term; type_ : _ term } -> typed term
  | TT_var : { var : Var.t } -> core term
  | TT_forall : { param : typed pat; return : _ term } -> core term
  | TT_lambda : { param : typed pat; return : _ term } -> core term
  | TT_apply : { lambda : _ term; arg : _ term } -> core term
  | TT_self : { bound : _ pat; body : _ term } -> core term
  | TT_fix : { bound : _ pat; body : _ term } -> core term
  | TT_unroll : { term : _ term } -> core term

and _ pat =
  | TP_loc : { pat : _ pat; loc : Location.t } -> loc pat
  | TP_typed : { pat : _ pat; type_ : _ term } -> typed pat
  | TP_var : { var : Var.t } -> core pat

type ex_term = Ex_term : _ term -> ex_term [@@ocaml.unboxed]
type ex_pat = Ex_pat : _ pat -> ex_pat [@@ocaml.unboxed]
