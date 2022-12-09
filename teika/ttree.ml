(* TODO: use hole var so that x => x, the hole var is something like _X
   to print as (x : _X) => x

   Maybe (x : ?X) => x *)

(* TODO: to avoid normalizing many times,
   makes normalization cached, with unification this
   means that this cache will probably be dependent on holes *)

type term =
  | TT_var of { offset : Offset.t }
  | TT_forall of { param : pat; return : term }
  | TT_lambda of { param : pat; return : term }
  | TT_apply of { lambda : term; arg : term }
  | TT_annot of { term : term; annot : term }
  | TT_offset of { term : term; offset : Offset.t }
  | TT_loc of { term : term; loc : Location.t [@opaque] }

and pat =
  | TP_var of { var : Name.t }
  | TP_annot of { pat : pat; annot : term }
  | TP_loc of { pat : pat; loc : Location.t [@opaque] }
[@@deriving show { with_path = false }]
