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
type term = { s_loc : Location.t; s_desc : term_desc }

and term_desc =
  | S_ident of identifier
  | S_lambda of { param : term; body : term }
  | S_apply of { lambda : term; arg : term }
  | S_bind of { bound : term; value : term; body : term option }
  | S_struct of term option
  | S_field of { struct_ : term; field : term }
  | S_match of { value : term; pat : term; body : term }
  | S_annot of { value : term; type_ : term }
(* TODO: term for importing and exporting? *)
(* TODO: extension point + attributes aka ppx *)
