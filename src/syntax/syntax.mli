type identifier = string

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

val value_from_string : string -> term option
