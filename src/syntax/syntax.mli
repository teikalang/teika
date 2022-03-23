type identifier = string

type term = { s_location : Location.t; s_description : term_description }

and term_description =
  | S_variable of identifier
  | S_lambda of { parameter : term; body : term }
  | S_apply of { lambda : term; argument : term }
  | S_binding of { pattern : term; value : term; body : term option }
  | S_structure of term option
  | S_field of { structure : term; field : term }
  | S_match of { value : term; pattern : term; body : term }
  | S_constraint of { value : term; type_ : term }

val value_from_string : string -> term option
