type loc = Loc
type typed = Typed
type core = Core
type sugar = Sugar

type _ term =
  | TT_loc : { term : _ term; loc : Location.t } -> loc term
  | TT_typed : { term : _ term; annot : _ term } -> typed term
  (* x/-n *)
  | TT_bound_var : { index : Index.t } -> core term
  (* x/+n *)
  | TT_free_var : { level : Level.t } -> core term
  (* (x : A) -> B *)
  | TT_forall : { var : Name.t; param : _ term; return : _ term } -> core term
  (* (x : A) => e *)
  | TT_lambda : { var : Name.t; param : _ term; return : _ term } -> core term
  (* l a *)
  | TT_apply : { lambda : _ term; arg : _ term } -> core term
  (* x = t; u *)
  | TT_let : { var : Name.t; value : _ term; return : _ term } -> sugar term
  (* (v : T) *)
  | TT_annot : { term : _ term; annot : _ term } -> sugar term

type ex_term = Ex_term : _ term -> ex_term [@@ocaml.unboxed]
