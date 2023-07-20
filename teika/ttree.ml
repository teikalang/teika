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
  | TT_hole : hole -> core term
  | TT_forall : { var : Name.t; param : _ term; return : _ term } -> core term
  | TT_lambda : { var : Name.t; param : _ term; return : _ term } -> core term
  | TT_apply : { lambda : _ term; arg : _ term } -> core term
  | TT_let : { var : Name.t; value : _ term; return : _ term } -> sugar term
  | TT_annot : { term : _ term; annot : _ term } -> sugar term

and hole = { mutable level : Level.t; mutable link : core term }

type ex_term = Ex_term : _ term -> ex_term [@@ocaml.unboxed]

let nil_level = Level.zero
let type_level = Level.next nil_level
let tt_nil = TT_free_var { level = nil_level }
let tt_type = TT_free_var { level = type_level }
