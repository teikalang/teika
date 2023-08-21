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
  | TT_subst : { subst : subst; term : _ term } -> subst term
  | TT_bound_var : { index : Index.t } -> core term
  | TT_free_var : { level : Level.t; alias : _ term option } -> core term
  | TT_hole : { hole : ex_term hole } -> core term
  | TT_forall : { param : typed pat; return : _ term } -> core term
  | TT_lambda : { param : typed pat; return : _ term } -> core term
  | TT_apply : { lambda : _ term; arg : _ term } -> core term
  | TT_self : { var : core pat; body : _ term } -> core term
  | TT_fix : { var : core pat; body : _ term } -> core term
  | TT_unroll : { term : _ term } -> core term
  | TT_unfold : { term : _ term } -> sugar term
  | TT_frozen : { term : _ term } -> core term
  | TT_freeze : { term : _ term } -> core term
  | TT_unfreeze : { term : _ term } -> core term
  | TT_let : { bound : _ pat; value : _ term; return : _ term } -> sugar term
  | TT_annot : { term : _ term; annot : _ term } -> sugar term
  | TT_string : { literal : string } -> core term
  | TT_native : { native : native } -> core term

and _ pat =
  (* TODO: TP_loc *)
  | TP_typed : { pat : _ pat; annot : _ term } -> typed pat
  | TP_hole : { hole : core pat hole } -> core pat
  | TP_var : { name : Name.t } -> core pat

and 'a hole = { mutable link : 'a }

and subst =
  | TS_subst_bound : { from : Index.t; to_ : _ term } -> subst
  | TS_subst_free : { from : Level.t; to_ : _ term } -> subst
  | TS_open_bound : { from : Index.t; to_ : Level.t } -> subst
  | TS_close_free : { from : Level.t; to_ : Index.t } -> subst

and native = TN_debug
and ex_term = Ex_term : _ term -> ex_term [@@ocaml.unboxed]
and ex_pat = Ex_pat : _ term -> ex_pat [@@ocaml.unboxed]
and ex_hole = Ex_hole : _ hole -> ex_hole [@@ocaml.unboxed]

let nil_level = Level.zero
let type_level = Level.next nil_level
let string_level = Level.next type_level

let tt_subst_bound ~from ~to_ term =
  TT_subst { subst = TS_subst_bound { from; to_ }; term }

let tt_subst_free ~from ~to_ term =
  TT_subst { subst = TS_subst_free { from; to_ }; term }

let tt_open_bound ~from ~to_ term =
  TT_subst { subst = TS_open_bound { from; to_ }; term }

let tt_close_free ~from ~to_ term =
  TT_subst { subst = TS_close_free { from; to_ }; term }

let tt_nil = TT_free_var { level = nil_level; alias = None }
let tt_type = TT_free_var { level = type_level; alias = None }
let string_type = TT_free_var { level = string_level; alias = None }

let tt_hole () =
  let hole = { link = Ex_term tt_nil } in
  TT_hole { hole }

let is_tt_nil (type a) (term : a term) =
  (* TODO: why not physical equality?
      Because TT_free_var is rebuilt in a couple places *)
  match term with
  | TT_free_var { level; alias = None } -> Level.equal nil_level level
  | _ -> false

(* TODO: ugly hack *)
let nil_name = Name.make "**nil**"

let tp_nil =
  (* TODO: very distinct way, maybe level on pattern? *)
  TP_var { name = nil_name }

let tp_hole () =
  let hole = { link = tp_nil } in
  TP_hole { hole }

let is_tp_nil (type a) (pat : a pat) =
  match pat with
  | TP_var { name } ->
      (* TODO: physical equality used here *)
      name == nil_name
  | _ -> false
