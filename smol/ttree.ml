type ty_term = TT_typed of { term : term; type_ : term }

and term =
  | TT_var of { var : Var.t }
  | TT_forall of { param : ty_pat; return : term }
  | TT_lambda of { param : ty_pat; return : term }
  | TT_apply of { lambda : term; arg : term }
  | TT_self of { bound : pat; body : term }
  | TT_fix of { bound : ty_pat; body : term }
  | TT_unroll of { term : term }
  | TT_expand of { term : term }

and ty_pat = TP_typed of { pat : pat; type_ : term }
and pat = TP_var of { var : Var.t }
