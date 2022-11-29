(* TODO: use hole var so that x => x, the hole var is something like _X
   to print as (x : _X) => x

   Maybe (x : ?X) => x *)

type term =
  | TTerm of { loc : Location.t; [@opaque] desc : term_desc; type_ : type_ }

and type_ = TType of { loc : Location.t; [@opaque] desc : term_desc }

and term_desc =
  | TT_var of { offset : Offset.t }
  | TT_forall of { param : pat; return : type_ }
  | TT_lambda of { param : pat; return : term }
  | TT_apply of { lambda : term; arg : term }
  | TT_exists of { left : annot; right : annot }
  | TT_pair of { left : bind; right : bind }
  | TT_let of { bound : bind; return : term }
  | TT_annot of { value : term; annot : type_ }
  | TT_offset of { desc : term_desc; offset : Offset.t }

and pat =
  | TPat of { loc : Location.t; [@opaque] desc : pat_desc; type_ : type_ }

and pat_desc =
  | TP_var of { var : Name.t }
  | TP_pair of { left : pat; right : pat }
  | TP_annot of { pat : pat; annot : type_ }

and annot = TAnnot of { loc : Location.t; [@opaque] pat : pat; annot : type_ }

and bind = TBind of { loc : Location.t; [@opaque] pat : pat; value : term }
[@@deriving show { with_path = false }]

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
let tt_let loc type_ ~bound ~return = tterm loc type_ (TT_let { bound; return })
let tt_annot loc ~value ~annot = tterm loc annot (TT_annot { value; annot })

(* pattern *)
let tpat loc type_ desc = TPat { loc; desc; type_ }
let tp_var loc type_ ~var = tpat loc type_ (TP_var { var })
let tp_pair loc type_ ~left ~right = tpat loc type_ (TP_pair { left; right })
let tp_annot loc ~pat ~annot = tpat loc annot (TP_annot { pat; annot })

(* annot *)
let tannot loc ~pat ~annot = TAnnot { loc; pat; annot }

(* bind *)
let tbind loc ~pat ~value = TBind { loc; pat; value }
