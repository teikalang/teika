(* TODO: use hole var so that x => x, the hole var is something like _X
   to print as (x : _X) => x

   Maybe (x : ?X) => x *)

(* TODO: to avoid normalizing many times,
   makes normalization cached, with unification this
   means that this cache will probably be dependent on holes *)

type loc = Loc
type offset = Offset
type annot = Annot
type core = Core

type _ term =
  | TT_loc : { term : _ term; loc : Location.t } -> loc term
  | TT_var : { offset : Offset.t } -> core term
  | TT_forall : { param : annot pat; return : _ term } -> core term
  | TT_lambda : { param : annot pat; return : _ term } -> core term
  | TT_apply : { lambda : _ term; arg : _ term } -> core term
  | TT_annot : { term : _ term; annot : _ term } -> annot term

and _ pat =
  | TP_loc : { pat : _ pat; loc : Location.t } -> loc pat
  | TP_var : { var : Name.t } -> core pat
  | TP_annot : { pat : _ pat; annot : _ term } -> annot pat

type ex_term = Ex_term : _ term -> ex_term [@@ocaml.unboxed]
type ex_pat = Ex_pat : _ pat -> ex_pat [@@ocaml.unboxed]
