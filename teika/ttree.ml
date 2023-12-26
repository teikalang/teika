open Syntax

(* TODO: use hole var so that x => x, the hole var is something like _X
   to print as (x : _X) => x

   Maybe (x : ?X) => x *)

(* TODO: to avoid normalizing many times,
   makes normalization cached, with unification this
   means that this cache will probably be dependent on holes *)

(* TODO: try parametric hoas *)

type term =
  | TT_bound_var of { index : Index.t }
  | TT_free_var of { level : Level.t }
  | TT_forall of { param : typed_pat; return : term }
  | TT_lambda of { param : typed_pat; return : term }
  | TT_apply of { lambda : term; arg : term }
  | TT_let of { bound : typed_pat; value : term; return : term }
  | TT_annot of { term : term; annot : term }
  | TT_string of { literal : string }
  | TT_native of { native : native }

and typed_pat = TPat of { pat : core_pat; type_ : term }

and core_pat = (* x *)
  | TP_var of { name : Name.t }

and subst =
  | TS_id
  (* open *)
  | TS_open of { to_ : Level.t }
  (* close +l *)
  | TS_close of { from : Level.t }
  (* lift s *)
  | TS_lift of { subst : subst }
  (* s :: n *)
  | TS_cons of { subst : subst; next : subst }

and native = TN_debug [@@deriving show { with_path = true }]

let nil_level = Level.zero
let type_level = Level.next nil_level
let string_level = Level.next type_level

(* TODO: loc *)
let tt_type = TT_free_var { level = type_level }
let string_type = TT_free_var { level = string_level }
