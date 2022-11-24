(* TODO: use hole var so that x => x, the hole var is something like _X
   to print as (x : _X) => x

   Maybe (x : ?X) => x *)

type term = TTerm of { loc : Location.t; desc : term_desc; type_ : type_ }
and type_ = TType of { loc : Location.t; desc : term_desc }

and term_desc =
  | TT_var of { offset : Offset.t }
  | TT_forall of { param : annot; return : type_ }
  | TT_lambda of { param : annot; return : term }
  | TT_apply of { lambda : term; arg : term }
  | TT_exists of { left : annot; right : annot }
  | TT_pair of { left : bind; right : bind }
  | TT_unpair of { left : Name.t; right : Name.t; pair : term; return : term }
  | TT_let of { bound : bind; return : term }
  | TT_annot of { value : term; annot : type_ }

and annot = TAnnot of { loc : Location.t; var : Name.t; annot : type_ }
and bind = TBind of { loc : Location.t; var : Name.t; value : term }

(* term *)
let tterm loc type_ desc = TTerm { loc; desc; type_ }
let ttype loc desc = TType { loc; desc }
let tt_var loc type_ ~offset = tterm loc type_ (TT_var { offset })
let tt_forall loc ~param ~return = ttype loc (TT_forall { param; return })

let tt_lambda loc type_ ~param ~return =
  tterm loc type_ (TT_lambda { param; return })

let tt_apply loc type_ ~lambda ~arg = tterm loc type_ (TT_apply { lambda; arg })
let tt_exists loc ~left ~right = ttype loc (TT_exists { left; right })
let tt_pair loc type_ ~left ~right = tterm loc type_ (TT_pair { left; right })

let tt_unpair loc type_ ~left ~right ~pair ~return =
  tterm loc type_ (TT_unpair { left; right; pair; return })

let tt_let loc type_ ~bound ~return = tterm loc type_ (TT_let { bound; return })
let tt_annot loc ~value ~annot = tterm loc annot (TT_annot { value; annot })

(* annot *)
let tannot loc ~var ~annot = TAnnot { loc; var; annot }

(* bind *)
let tbind loc ~var ~value = TBind { loc; var; value }
