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
  (* TODO: this alias is a hack *)
  | TT_free_var : { level : Level.t; alias : _ term option } -> core term
  (* TODO: I really don't like this ex_term *)
  (* _x/+n *)
  | TT_hole : { hole : ex_term hole } -> core term
  (* (x : A) -> B *)
  | TT_forall : { param : typed pat; return : _ term } -> core term
  (* (x : A) => e *)
  | TT_lambda : { param : typed pat; return : _ term } -> core term
  (* l a *)
  | TT_apply : { lambda : _ term; arg : _ term } -> core term
  (* @self(x -> e)*)
  (* TODO: why core pat? *)
  | TT_self : { var : core pat; body : _ term } -> core term
  (* @fix(x => e)*)
  (* TODO: why core pat? *)
  | TT_fix : { var : core pat; body : _ term } -> core term
  (* @unroll(e)*)
  | TT_unroll : { term : _ term } -> core term
  (* @unfold(e)*)
  (* TODO: technically not sugar *)
  | TT_unfold : { term : _ term } -> sugar term
  (* x = t; u *)
  | TT_let : { bound : _ pat; value : _ term; return : _ term } -> sugar term
  (* (v : T) *)
  | TT_annot : { term : _ term; annot : _ term } -> sugar term

and _ pat =
  | TP_typed : { pat : _ pat; annot : _ term } -> typed pat
  | TP_hole : { hole : core pat hole } -> core pat
  (* x *)
  | TP_var : { name : Name.t } -> core pat

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
and ex_pat = Ex_pat : _ term -> ex_pat [@@ocaml.unboxed]
and ex_hole = Ex_hole : _ hole -> ex_hole [@@ocaml.unboxed]

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
val tp_nil : core pat
val tp_hole : unit -> core pat
val is_tp_nil : _ pat -> bool
