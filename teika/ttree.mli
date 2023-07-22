type loc = Loc
type subst = Subst
type typed = Typed
type core = Core
type sugar = Sugar

type _ term =
  | TT_loc : { term : _ term; loc : Location.t } -> loc term
  | TT_typed : { term : _ term; annot : _ term } -> typed term
  (* M[-n := N]*)
  | TT_subst_bound : {
      from : Index.t;
      to_ : _ term;
      term : _ term;
    }
      -> subst term
  (* M[+f `close` -t]*)
  | TT_close_free : {
      from : Level.t;
      to_ : Index.t;
      term : _ term;
    }
      -> subst term
  (* x/-n *)
  | TT_bound_var : { index : Index.t } -> core term
  (* x/+n *)
  | TT_free_var : { level : Level.t } -> core term
  (* _x/+n *)
  | TT_hole : hole -> core term
  (* (x : A) -> B *)
  | TT_forall : { param : _ term; return : _ term } -> core term
  (* (x : A) => e *)
  | TT_lambda : { param : _ term; return : _ term } -> core term
  (* l a *)
  | TT_apply : { lambda : _ term; arg : _ term } -> core term
  (* x = t; u *)
  | TT_let : { value : _ term; return : _ term } -> sugar term
  (* (v : T) *)
  | TT_annot : { term : _ term; annot : _ term } -> sugar term

and hole = { mutable link : core term }

type ex_term = Ex_term : _ term -> ex_term [@@ocaml.unboxed]

val nil_level : Level.t
val type_level : Level.t

(* constructors *)
val tt_subst_bound : from:Index.t -> to_:_ term -> _ term -> subst term
val tt_close_free : from:Level.t -> to_:Index.t -> _ term -> subst term
val tt_nil : core term
val tt_type : core term
val tt_hole : unit -> core term
