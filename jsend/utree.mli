type term =
  (* TODO: why is loc a term? *)
  | UT_loc of { term : term; loc : Location.t }
  | UT_var of { var : Var.t }
  (* TODO: patterns in the Itree? *)
  | UT_lambda of { param : Var.t; return : term }
  | UT_apply of { lambda : term; arg : term }
  | UT_let of { var : Var.t; value : term; return : term }
  | UT_string of { literal : string }
  | UT_external of { external_ : external_ }

and external_ = UE_type | UE_fix | UE_unit | UE_debug
