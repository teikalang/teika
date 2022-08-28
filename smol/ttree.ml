type expr = TE of { type_ : Type.t; desc : expr_desc }

and expr_desc =
  | TE_var of { var : Var.t }
  | TE_lambda of { var : Var.t; param : type_; return : expr }
  | TE_forall of { var : Var.t; return : expr }
  | TE_apply of { funct : expr; arg : expr }
  | TE_pair of { left : bind; right : bind }
  | TE_exits of { var : Var.t; right : bind }
  | TE_unpair of { unpair : unpair; return : expr }
  | TE_type of { type_ : type_ }
  | TE_let of { bind : bind; return : expr }
  | TE_annot of { expr : expr; type_ : type_ }

and bind = TB of { type_ : Type.t; var : Var.t; value : expr }

and unpair =
  | TU of { type_ : Type.t; left : Var.t; right : Var.t; value : expr }

and type_ = TT of { type_ : Type.t; desc : type_desc }

and type_desc =
  | TT_var of { var : Var.t }
  | TT_arrow of { param : type_; return : type_ }
  | TT_forall of { var : Var.t; return : type_ }
  | TT_pair of { left : type_; right : type_ }
  | TT_exists of { var : Var.t; right : type_ }

(* helpers *)
open Type

(* QUESTION: Why only some of the functions expect type_?
   ANSWER: Some expressions always have the same type_ and can easily
    be synthethised by combining the type of its components. *)

(* expr *)
let te type_ desc = TE { type_; desc }
let te_var type_ ~var = te type_ (TE_var { var })

let te_lambda ~var ~param ~return =
  let type_ =
    let (TT { type_ = param; desc = _ }) = param in
    let (TE { type_ = return; desc = _ }) = return in
    t_arrow ~var ~param ~return
  in
  te type_ (TE_lambda { var; param; return })

let te_forall ~var ~return =
  let type_ =
    let (TE { type_ = return; desc = _ }) = return in
    let param = t_type in
    t_arrow ~var ~param ~return
  in
  te type_ (TE_forall { var; return })

let te_apply type_ ~funct ~arg = te type_ (TE_apply { funct; arg })

let te_pair ~left ~right =
  let type_ =
    let (TB { type_ = left; var = _; value = _ }) = left in
    let (TB { type_ = right; var = _; value = _ }) = right in
    t_pair ~left ~right
  in
  te type_ (TE_pair { left; right })

let te_exists ~var ~right =
  let type_ =
    let (TB { type_ = right; var = _; value = _ }) = right in
    t_exists ~var ~right
  in
  te type_ (TE_exits { var; right })

let te_unpair ~unpair ~return =
  let (TE { type_; desc = _ }) = return in
  te type_ (TE_unpair { unpair; return })

let te_type ~type_ =
  let type_type =
    let (TT { type_; desc = _ }) = type_ in
    t_alias ~type_
  in
  te type_type (TE_type { type_ })

let te_let ~bind ~return =
  let (TE { type_; desc = _ }) = return in
  te type_ (TE_let { bind; return })

let te_annot ~expr ~type_ =
  let (TT { type_ = type_type; desc = _ }) = type_ in
  te type_type (TE_annot { expr; type_ })

(* bind *)
let tb ~var ~value =
  let (TE { type_; desc = _ }) = value in
  TB { type_; var; value }

(* unpair *)
let tu ~left ~right ~value =
  let (TE { type_; desc = _ }) = value in
  TU { type_; left; right; value }

(* type *)
let tt type_ desc = TT { type_; desc }
let tt_var type_ ~var = tt type_ (TT_var { var })

let tt_arrow ~param ~return =
  let type_ =
    (* TODO: this is weird *)
    let var = Var.create (Name.make "_") in
    let (TT { type_ = param; desc = _ }) = param in
    let (TT { type_ = return; desc = _ }) = return in
    t_arrow ~var ~param ~return
  in
  tt type_ (TT_arrow { param; return })

let tt_forall ~var ~return =
  let type_ =
    let (TT { type_ = return; desc = _ }) = return in
    let param = t_type in
    t_arrow ~var ~param ~return
  in
  tt type_ (TT_forall { var; return })

let tt_pair ~left ~right =
  let type_ =
    let (TT { type_ = left; desc = _ }) = left in
    let (TT { type_ = right; desc = _ }) = right in
    t_pair ~left ~right
  in
  tt type_ (TT_pair { left; right })

let tt_exists ~var ~right =
  let type_ =
    let (TT { type_ = right; desc = _ }) = right in
    t_exists ~var ~right
  in
  tt type_ (TT_exists { var; right })
