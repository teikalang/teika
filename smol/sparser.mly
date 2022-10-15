%{
open Stree

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
%token LEFT_PARENS (* ( *)
%token RIGHT_PARENS (* ) *)

%token EOF

%start <Stree.term option> term_opt

%%

(* difference between term and type is just the precedence *)
let term_opt :=
  | EOF;
    { None }
  | term = term; EOF;
    { Some term }


let term := term_rec_semi

let term_rec_semi :=
  | term_rec_bind
  | term_semi(term_rec_semi, term_rec_bind)

let term_rec_bind :=
  | term_rec_funct
  | term_bind(term_rec_funct, term_rec_funct)

let term_rec_funct :=
  | term_rec_apply
  | term_arrow(term_rec_funct, term_rec_apply)
  | term_lambda(term_rec_funct, term_rec_apply)

let term_rec_apply :=
  | term_atom
  | term_apply(term_rec_apply, term_atom)

let term_atom :=
  | term_type
  | term_var
  | term_parens(term_parens_maybe_pair)

let term_parens_maybe_pair :=
  | term_parens_maybe_annot
  | term_pair(term_parens_maybe_pair, term_parens_maybe_annot)

let term_parens_maybe_annot :=
  | term
  | term_annot(term_parens_maybe_annot, term)

let term_type ==
  | ASTERISK;
    { st_type (mk $loc) }
let term_var ==
  | var = VAR;
    { st_var (mk $loc) ~var:(Name.make var) }
let term_arrow(self, lower) ==
  | param = lower; ARROW; return = self;
    { st_arrow (mk $loc) ~param ~return }
let term_lambda(self, lower) ==
  | param = lower; FAT_ARROW; return = self;
    { st_lambda (mk $loc) ~param ~return }
let term_apply(self, lower) ==
  | lambda = self; arg = lower;
    { st_apply (mk $loc) ~lambda ~arg }
let term_pair(self, lower) ==
  | left = lower; COMMA; right = self;
    { st_pair (mk $loc) ~left ~right }
let term_bind(self, lower) ==
  | bound = lower; EQUAL; value = self;
    { st_bind (mk $loc) ~bound ~value }
let term_semi(self, lower) ==
  | left = lower; SEMICOLON; right = self;
    { st_semi (mk $loc) ~left ~right }
let term_annot(self, lower) ==
  | value = lower; COLON; type_ = self;
    { st_annot (mk $loc) ~value ~type_ }
let term_parens(content) ==
  | LEFT_PARENS; term = content; RIGHT_PARENS;
    { term }
