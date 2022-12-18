type loc = Loc
type typed = Typed
type core = Core
type sugar = Sugar

type _ term =
  | TT_loc : { term : _ term; loc : Location.t } -> loc term
  | TT_typed : { term : _ term; annot : _ term } -> typed term
  (* x *)
  | TT_var : { offset : Offset.t } -> core term
  (* (x : A) -> B *)
  | TT_forall : { param : typed pat; return : _ term } -> core term
  (* (x : A) => e *)
  | TT_lambda : { param : typed pat; return : _ term } -> core term
  (* l a *)
  | TT_apply : { lambda : _ term; arg : _ term } -> core term
  (* (v : T) *)
  | TT_annot : { term : _ term; annot : _ term } -> sugar term

and _ pat =
  | TP_loc : { pat : _ pat; loc : Location.t } -> loc pat
  | TP_typed : { pat : _ pat; annot : _ term } -> typed pat
  (* x *)
  | TP_var : { var : Name.t } -> core pat
  (* (p : T) *)
  | TP_annot : { pat : _ pat; annot : _ term } -> sugar pat

type ex_term = Ex_term : _ term -> ex_term [@@ocaml.unboxed]
type ex_pat = Ex_pat : _ pat -> ex_pat [@@ocaml.unboxed]
