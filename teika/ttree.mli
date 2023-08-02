type loc = Loc
type typed = Typed
type core = Core
type sugar = Sugar

type _ term =
  | TT_loc : { term : _ term; loc : Location.t } -> loc term
  | TT_typed : { term : _ term; annot : _ term } -> typed term
  (* M[S]*)
  | TT_subst : { subst : subst; term : _ term } -> subst term
  (* x/-n *)
  | TT_bound_var : { index : Index.t } -> core term
  (* x/+n *)
  | TT_free_var : { level : Level.t } -> core term
  (* _x/+n *)
  (* TODO: I really don't like this ex_term *)
  | TT_hole : { hole : ex_term hole } -> core term
  (* (x : A) -> B *)
  | TT_forall : { param : _ term; return : _ term } -> core term
  (* (x : A) => e *)
  | TT_lambda : { param : _ term; return : _ term } -> core term
  (* l a *)
  | TT_apply : { lambda : _ term; arg : _ term } -> core term
  (* @self(x -> e)*)
  | TT_self : { body : _ term } -> core term
  (* @fix(x => e)*)
  | TT_fix : { body : _ term } -> core term
  (* @unroll(e)*)
  | TT_unroll : { term : _ term } -> core term
  (* @unfold(e)*)
  (* TODO: technically not sugar *)
  | TT_unfold : { term : _ term } -> sugar term
  (* x = t; u *)
  | TT_let : { value : _ term; return : _ term } -> sugar term
  (* (v : T) *)
  | TT_annot : { term : _ term; annot : _ term } -> sugar term

and 'a hole = { mutable link : 'a }

and subst =
  (* -f := N *)
  | TS_subst_bound : { from : Index.t; to_ : _ term } -> subst
  (* +f := N *)
  | TS_subst_free : { from : Level.t; to_ : _ term } -> subst
  (* -f `open` +t *)
  | TS_open_bound : { from : Index.t; to_ : Level.t } -> subst
  (* +f `open` -t *)
  | TS_close_free : { from : Level.t; to_ : Index.t } -> subst

and ex_term = Ex_term : _ term -> ex_term [@@ocaml.unboxed]

val nil_level : Level.t
val type_level : Level.t

(* constructors *)
val tt_subst_bound : from:Index.t -> to_:_ term -> _ term -> subst term
val tt_subst_free : from:Level.t -> to_:_ term -> _ term -> subst term
val tt_open_bound : from:Index.t -> to_:Level.t -> _ term -> subst term
val tt_close_free : from:Level.t -> to_:Index.t -> _ term -> subst term
val tt_nil : core term
val tt_type : core term
val tt_hole : unit -> core term
val is_tt_nil : _ term -> bool
