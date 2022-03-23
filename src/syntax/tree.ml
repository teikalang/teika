(** TODO: document variants tagging, module first letter + type first letter*)

(* TODO:

   Terminology

   Parameter = the hole (parameter -> ())
   Argument = the value (f argument)

   ??? Function / Arrow / Lambda / Abstraction
   ??? Call / Apply / Application

   ??? body / return when talking about arrow and forall

   ??? variable / parameter when talking about forall

   ??? variable / identifier

   ??? constraint / annotation on types

   ??? module / structure

   ??? Let / Binding

   ??? record / structure
*)

(* TODO: location *)
type identifier = string [@@deriving show]

(* TODO: likely simple AST where let and module bindings are fused *)
(* TODO: maybe make each one of those in it's own modules? *)
type term = { s_location : Location.t; s_description : term_description }

and term_description =
  | S_variable of identifier
  | S_lambda of { parameter : term; body : term }
  | S_apply of { lambda : term; argument : term }
  (* TODO: binding or let *)
  (* TODO: is being recursive a good idea? *)
  | S_binding of { pattern : term; value : term; body : term option }
  | S_structure of term option
  | S_field of { structure : term; field : term }
  (* TODO: is being recursive a good idea? *)
  | S_match of { value : term; pattern : term; body : term }
  | S_constraint of { value : term; type_ : term }
(* TODO: term for importing and exporting? *)
(* TODO: extension point + attributes aka ppx *)
