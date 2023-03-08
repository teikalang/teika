type loc = Location
type typed = Typed
type core = Core

type _ term =
  | TT_var : { var : Var.t } -> core term
  | TT_forall : { param : typed pat; return : _ term } -> core term
  | TT_lambda : { param : typed pat; return : _ term } -> core term
  | TT_apply : { lambda : _ term; arg : _ term } -> core term
  | TT_self : { bound : _ pat; body : _ term } -> core term
  (* TODO: this typed term on the body is probably not needed
      but this project tries to play very safe *)
  | TT_fix : { bound : typed pat; body : _ term } -> core term
  | TT_unroll : { term : _ term } -> core term

and _ pat =
  | TP_typed : { pat : _ pat; type_ : _ term } -> typed pat
  | TP_var : { var : Var.t } -> core pat

type ty_term = TT_typed : { term : _ term; type_ : _ term } -> ty_term
type ex_term = Ex_term : _ term -> ex_term [@@ocaml.unboxed]
type ex_pat = Ex_pat : _ pat -> ex_pat [@@ocaml.unboxed]
