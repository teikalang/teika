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

type term =
  | TTerm of { desc : term_desc; type_ : term }
  | TType of { desc : term_desc }

and term_desc =
  | TT_subst of { term : term; subst : subst }
  | TT_bound_var of { index : Index.t }
  | TT_free_var of { level : Level.t; alias : term option }
  | TT_hole of { hole : term hole }
  | TT_forall of { param : typed_pat; return : term }
  | TT_lambda of { param : typed_pat; return : term }
  | TT_apply of { lambda : term; arg : term }
  | TT_self of { var : core_pat; body : term }
  | TT_fix of { var : core_pat; body : term }
  | TT_unroll of { term : term }
  | TT_unfold of { term : term }
  | TT_let of { bound : typed_pat; value : term; return : term }
  | TT_annot of { term : term; annot : term }
  | TT_string of { literal : string }
  | TT_native of { native : native }

and typed_pat = TPat of { pat : core_pat; type_ : term }

and core_pat =
  | TP_hole of { hole : core_pat hole }
  (* x *)
  | TP_var of { name : Name.t }

and 'a hole = { mutable link : 'a option }

and subst =
  | TS_subst_bound of { from : Index.t; to_ : term }
  | TS_subst_free of { from : Level.t; to_ : term }
  | TS_open_bound of { from : Index.t; to_ : Level.t }
  | TS_close_free of { from : Level.t; to_ : Index.t }

and native = TN_debug

let nil_level = Level.zero
let type_level = Level.next nil_level
let string_level = Level.next type_level

(* TODO: path compression *)

let rec tp_repr pat =
  match pat with
  | TP_hole { hole } -> (
      match hole.link with Some pat -> tp_repr pat | None -> pat)
  | TP_var _ -> pat

let rec tt_repr term =
  match term with
  (* TODO: expand type_? *)
  | TTerm { desc; type_ = _ } -> tt_repr_desc term desc
  | TType { desc } -> tt_repr_desc term desc

and tt_repr_desc term desc =
  match desc with
  | TT_hole { hole } -> (
      match hole.link with Some term -> tt_repr term | None -> term)
  (* TODO: expand cases here  *)
  | _ -> term

let tt_match term =
  match tt_repr term with
  (* TODO: expand type_? *)
  | TTerm { desc; type_ = _ } -> desc
  | TType { desc } -> desc

let tt_map_desc term f =
  match tt_repr term with
  (* TODO: expand type_? *)
  | TTerm { desc; type_ } ->
      let wrap desc = TTerm { desc; type_ } in
      f ~wrap term desc
  | TType { desc } ->
      let wrap desc = TType { desc } in
      f ~wrap term desc

(* TODO: loc *)
let tt_type =
  (* TODO: why types have locations? *)
  let desc = TT_free_var { level = type_level; alias = None } in
  TType { desc }

let string_type =
  let desc = TT_free_var { level = string_level; alias = None } in
  TType { desc }

let tt_hole () =
  let hole = { link = None } in
  TT_hole { hole }

let tp_hole () =
  let hole = { link = None } in
  TP_hole { hole }
