type loc = Loc
type offset = Offset
type annot = Annot
type core = Core

type _ term =
  | TT_loc : { term : _ term; loc : Location.t } -> loc term
  | TT_offset : { term : _ term; offset : Offset.t } -> offset term
  (* x *)
  | TT_var : { offset : Offset.t } -> core term
  (* (x : A) -> B *)
  | TT_forall : { param : annot pat; return : _ term } -> core term
  (* (x : A) => e *)
  | TT_lambda : { param : annot pat; return : _ term } -> core term
  (* l a *)
  | TT_apply : { lambda : _ term; arg : _ term } -> core term
  (* (v : T) *)
  | TT_annot : { term : _ term; annot : _ term } -> annot term

and _ pat =
  | TP_loc : { pat : _ pat; loc : Location.t } -> loc pat
  (* x *)
  | TP_var : { var : Name.t } -> core pat
  (* (p : T) *)
  | TP_annot : { pat : _ pat; annot : _ term } -> annot pat

type ex_term = Ex_term : _ term -> ex_term [@@ocaml.unboxed]
type ex_pat = Ex_pat : _ pat -> ex_pat [@@ocaml.unboxed]
