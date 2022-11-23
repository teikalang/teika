open Ttree

(* TODO: maybe call it var_desc? *)

(** This describes which kind of variable are we dealing with
      - V_rigid are universal variables, they cannot move
      - V_alias are used when binding two rigid variables
          it is also used when unifying two hole variables
      - V_link are hole variables that were unified 
      - V_hole are existential variables, they can move 
          they can also be linked *)
type var_info = private
  | V_rigid of { level : Level.t }
  | V_alias of { var : Var.t }
  (* TODO: this shouldy be type_ : type_ *)
  | V_link of { type_ : term_desc }
  | V_hole of { level : Level.t }

type error = private CError of { loc : Location.t; desc : error_desc }

and error_desc = private
  (* unification *)
  | CError_occurs_check of { var : Var.t; in_ : Var.t }
  | CError_escape_check of { var : Var.t; to_ : Var.t }
  | CError_var_constrained of { var : Var.t; by_ : term_desc }
  | CError_var_clash of { expected : Var.t; received : Var.t }
  | CError_type_clash of { expected : term_desc; received : term_desc }
  (* invariants *)
  | CError_unknown_var_repr of { var : Var.t }
  | CError_unknown_var_link of { var : Var.t }
  | CError_unknown_var_lower of { var : Var.t }
  | CError_unknown_var_alias of { var : Var.t }
  | CError_duplicated_var_rigid of { var : Var.t }
  | CError_duplicated_var_alias of { var : Var.t }
  | CError_duplicated_var_hole of { var : Var.t }
  | CError_invalid_var_link of { var : Var.t; info : var_info }
  | CError_invalid_var_lower of { var : Var.t; info : var_info }
  | CError_invalid_var_alias of { var : Var.t; info : var_info }
  | CError_lowering_to_higher_level of { var : Var.t }

type 'a context
type 'a t = 'a context

val return : 'a -> 'a context
val ( >>= ) : 'a context -> ('a -> 'b context) -> 'b context
val fail_occurs_check : Var.t -> in_:Var.t -> 'a context
val fail_escape_check : Var.t -> to_:Var.t -> 'a context
val fail_var_constrained : Var.t -> by_:term_desc -> 'a context
val fail_var_clash : expected:Var.t -> received:Var.t -> 'a context
val fail_type_clash : expected:term_desc -> received:term_desc -> 'a context

(* read *)
val loc : Location.t context
val level : Level.t context
val repr : Var.t -> var_info context

(* update *)
val with_loc : Location.t -> (unit -> 'a context) -> 'a context
val with_region : (unit -> 'a context) -> 'a context
val with_var_rigid : Var.t -> (unit -> 'a context) -> 'a context
val with_var_alias : Var.t -> of_:Var.t -> (unit -> 'a context) -> 'a context

(* unification *)
val enter_var_hole : Var.t -> unit context
val var_link : Var.t -> to_:term_desc -> unit context
val var_lower : Var.t -> to_:Level.t -> unit context
val var_alias : Var.t -> of_:Var.t -> unit context
(* TODO: generalize *)
