%{
open Ltree

let mk (loc_start, loc_end) =
  Location.{ loc_start; loc_end; loc_ghost = false }

%}
%token <string> VAR (* x *)
%token ARROW (* -> *)
%token FAT_ARROW (* => *)
%token EQUAL (* = *)
%token COMMA (* , *)
%token COLON (* : *)
%token SEMICOLON (* ; *)
%token ASTERISK (* * *)
%token HASH_TAG (* # *)
%token LEFT_PARENS (* ( *)
%token RIGHT_PARENS (* ) *)

%token EOF

%start <Ltree.expr option> expr_opt
%start <Ltree.type_ option> type_opt

%%

(* difference between term and type is just the precedence *)
let expr_opt :=
  | EOF;
    { None }
  | expr = expr; EOF;
    { Some expr }
let type_opt :=
  | EOF;
    { None }
  | type_ = type_; EOF;
    { Some type_ }


let expr ==
  | expr_funct_let

let expr_funct_let :=
  | expr_rec_apply
  | expr_lambda(expr_funct_let)
  | expr_forall(expr_funct_let)
  | expr_let(expr_funct_let)

let expr_rec_apply :=
  | expr_atom
  | expr_apply(expr_rec_apply)

(* TODO: this not being := is very weird *)
let expr_atom ==
  | expr_var
  | expr_type
  | expr_parens

let expr_parens :=
  | LEFT_PARENS; expr = expr; RIGHT_PARENS;
    { expr }
  | LEFT_PARENS; expr = expr_required_parens; RIGHT_PARENS;
    { expr }

let expr_required_parens ==
  | expr_pair
  | expr_exists
  | expr_annot

let var ==
  | var = VAR;
    { Name.make var }
let expr_var ==
  | var = var;
    { le_var (mk $loc) ~var }
let expr_lambda(self) ==
  | LEFT_PARENS; var = var; COLON; param = type_atom; RIGHT_PARENS;
    FAT_ARROW; return = self;
    { le_lambda (mk $loc) ~var ~param ~return }
let expr_forall(self) ==
  | LEFT_PARENS; var = var; COLON; ASTERISK; RIGHT_PARENS;
    FAT_ARROW; return = self;
    { le_forall (mk $loc) ~var ~return }
let expr_apply(self) ==
  | funct = self; arg = expr_atom;
    { le_apply (mk $loc) ~funct ~arg }
let expr_pair ==
  | left = bind; COMMA; right = bind;
    { le_pair (mk $loc) ~left ~right }
let expr_exists ==
  | var = var; COLON; ASTERISK; right = bind;
    { le_exists (mk $loc) ~var ~right }
let expr_type ==
  | HASH_TAG; type_ = type_atom;
    { le_type (mk $loc) ~type_ }
let expr_let(self) ==
  | bind = bind; SEMICOLON; return = self;
    { le_let (mk $loc) ~bind ~return }
let expr_annot ==
  (* TODO: expr and type_ can be more general *)
  | expr = expr_atom; COLON; type_ = type_atom;
    { le_annot (mk $loc) ~expr ~type_ }


let bind ==
  | var = var; EQUAL; value = expr;
    { lb (mk $loc) ~var ~value }

let type_ ==
  | type_funct

let type_funct :=
  | type_atom
  | type_arrow(type_funct)
  | type_forall(type_funct)

let type_atom :=
  | type_var
  | type_parens

let type_parens :=
  | LEFT_PARENS; type_ = type_; RIGHT_PARENS;
    { type_ }
  | LEFT_PARENS; type_ = type_required_parens; RIGHT_PARENS;
    { type_ }

let type_required_parens ==
  | type_pair
  | type_exists

let type_var ==
  | var = var;
    { lt_var (mk $loc) ~var }
let type_arrow(self) ==
  | param = type_atom; ARROW; return = self;
    { lt_arrow (mk $loc) ~param ~return }
let type_forall(self) ==
  | LEFT_PARENS; var = var; COLON; ASTERISK; RIGHT_PARENS;
    ARROW; return = self;
    { lt_forall (mk $loc) ~var ~return }
let type_pair ==
  | _ = var; COLON; left = type_; COMMA; var; COLON; right = type_;
    { lt_pair (mk $loc) ~left ~right }
let type_exists ==
  | var = var; COLON; ASTERISK; COMMA; var; COLON; right = type_;
    { lt_exists (mk $loc) ~var ~right }
