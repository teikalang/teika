open Ttree
open Terror

type var_info = Free | Subst of term
type 'a context
type 'a t = 'a context

(* monad *)
val run : (unit -> 'a context) -> ('a, error) result

(* TODO: next_var must be bigger than type_of_types *)
val test :
  level:Level.t ->
  vars:(Level.t * term * term option) Name.Map.t ->
  expected_vars:var_info list ->
  received_vars:var_info list ->
  (unit -> 'a context) ->
  ('a, error) result

val return : 'a -> 'a context
val bind : 'a context -> ('a -> 'b context) -> 'b context
val ( let* ) : 'a context -> ('a -> 'b context) -> 'b context
val ( let+ ) : 'a context -> ('a -> 'b) -> 'b context

(* errors *)
(* unify *)
val error_subst_found : expected:term -> received:term -> 'a context
val error_annot_found : expected:term -> received:term -> 'a context
val error_bound_var_clash : expected:Index.t -> received:Index.t -> 'a context
val error_free_var_clash : expected:Level.t -> received:Level.t -> 'a context
val error_type_clash : expected:term -> received:term -> 'a context
val error_var_occurs : hole:term hole -> in_:term hole -> 'a context
val error_string_clash : expected:string -> received:string -> 'a context

(* typer *)
val error_pairs_not_implemented : unit -> 'a context
val error_not_a_forall : type_:term -> 'a context
val error_var_escape : var:Level.t -> 'a context

val error_typer_unknown_extension :
  extension:Name.t -> payload:Ltree.term -> 'a context

val error_typer_unknown_native : native:string -> 'a context

(* level *)
val level : unit -> Level.t context
val enter_level : (unit -> 'a context) -> 'a context

(* vars *)
val lookup_var : name:Name.t -> (Level.t * term * term option) context

(* TODO: resolve not great  *)
val resolve_bound_var : index:Index.t -> term option context
val resolve_free_var : level:Level.t -> term option context
val with_expected_var : (unit -> 'a context) -> 'a context

val with_received_var :
  name:Name.t ->
  type_:term ->
  alias:term option ->
  (unit -> 'a context) ->
  'a context

(* locs *)
val with_loc : loc:Location.t -> (unit -> 'a context) -> 'a context
