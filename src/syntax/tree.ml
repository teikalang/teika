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
type syntax = { s_location : Location.t; s_description : syntax_description }

and syntax_description =
  | S_variable of identifier
  | S_lambda of { parameter : syntax; body : syntax }
  | S_apply of { lambda : syntax; argument : syntax }
  (* TODO: binding or let *)
  (* TODO: is being recursive a good idea? *)
  | S_binding of { pattern : syntax; value : syntax; body : syntax option }
  | S_structure of syntax option
  | S_field of { structure : syntax; field : syntax }
  (* TODO: is being recursive a good idea? *)
  | S_match of { value : syntax; pattern : syntax; body : syntax }
  | S_constraint of { value : syntax; type_ : syntax }
(* TODO: syntax for importing and exporting? *)
(* TODO: extension point + attributes aka ppx *)
