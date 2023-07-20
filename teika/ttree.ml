(* TODO: use hole var so that x => x, the hole var is something like _X
   to print as (x : _X) => x

   Maybe (x : ?X) => x *)

(* TODO: to avoid normalizing many times,
   makes normalization cached, with unification this
   means that this cache will probably be dependent on holes *)

(* TODO: try parametric hoas *)
type loc = Loc
type typed = Typed
type core = Core
type sugar = Sugar

type _ term =
  | TT_loc : { term : _ term; loc : Location.t } -> loc term
  | TT_typed : { term : _ term; annot : _ term } -> typed term
  | TT_bound_var : { index : Index.t } -> core term
  | TT_free_var : { level : Level.t } -> core term
  | TT_forall : { param : typed pat; return : _ term } -> core term
  | TT_lambda : { param : typed pat; return : _ term } -> core term
  | TT_apply : { lambda : _ term; arg : _ term } -> core term
  | TT_let : { pat : typed pat; value : _ term; return : _ term } -> sugar term
  | TT_annot : { term : _ term; annot : _ term } -> sugar term

and _ pat =
  | TP_loc : { pat : _ pat; loc : Location.t } -> loc pat
  | TP_typed : { pat : _ pat; annot : _ term } -> typed pat
  | TP_var : { var : Name.t } -> core pat
  | TP_annot : { pat : _ pat; annot : _ term } -> sugar pat

type ex_term = Ex_term : _ term -> ex_term [@@ocaml.unboxed]
type ex_pat = Ex_pat : _ pat -> ex_pat [@@ocaml.unboxed]
