open Syntax
open Ttree
open Terror

val debug : bool ref

(* TODO: bad API *)
val raw_pp_term :
  bound_vars:Name.t list ->
  free_vars:Name.t Level.Map.t ->
  Format.formatter ->
  term ->
  unit

val raw_pp_term_hole :
  bound_vars:Name.t list ->
  free_vars:Name.t Level.Map.t ->
  Format.formatter ->
  term hole ->
  unit

val raw_pp_subst :
  bound_vars:Name.t list ->
  free_vars:Name.t Level.Map.t ->
  Format.formatter ->
  subst ->
  unit

val raw_pp_error :
  bound_vars:Name.t list ->
  free_vars:Name.t Level.Map.t ->
  Format.formatter ->
  error ->
  unit

val pp_term : Format.formatter -> term -> unit
val pp_term_hole : Format.formatter -> term hole -> unit
val pp_subst : Format.formatter -> subst -> unit
val pp_error : Format.formatter -> error -> unit
