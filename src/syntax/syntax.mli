type identifier = string

type syntax = { s_location : Location.t; s_description : syntax_description }

and syntax_description =
  | S_variable of identifier
  | S_lambda of { parameter : syntax; body : syntax }
  | S_apply of { lambda : syntax; argument : syntax }
  | S_forall of { parameter : syntax; body : syntax }
  | S_binding of { pattern : syntax; value : syntax; body : syntax option }
  | S_structure of syntax option
  | S_field of { structure : syntax; field : syntax }
  | S_constraint of { value : syntax; type_ : syntax }

val value_from_string : string -> syntax option
