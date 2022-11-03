%{
open Stree

let mk (loc_start, loc_end) =
  Location.{ loc_start; loc_end; loc_ghost = false }

%}
%token <string> VAR (* x *)
%token COLON (* : *)
%token ARROW (* -> *)
%token FAT_ARROW (* => *)
%token EQUAL (* = *)
%token COMMA (* , *)
%token AMPERSAND (* & *)
%token SEMICOLON (* ; *)
%token LEFT_PARENS (* ( *)
%token RIGHT_PARENS (* ) *)
%token LEFT_BRACE (* { *)
%token RIGHT_BRACE (* } *)

%token EOF

%start <Stree.term option> term_opt

%%

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
  | term_forall(term_rec_funct, term_rec_apply)
  | term_lambda(term_rec_funct, term_rec_apply)

let term_rec_apply :=
  | term_atom
  | term_apply(term_rec_apply, term_atom)

let term_atom :=
  | term_var
  | term_parens(term_wrapped)
  | term_braces(term_wrapped)

let term_wrapped :=
  | term
  | term_annot(term_rec_annot, term_rec_funct)
  | term_pair(term_rec_pair, term_rec_bind)
  | term_both(term_rec_both, term_rec_bind)

let term_rec_annot :=
  (* TODO: why not term_rec_bind? *)
  | term_rec_funct
  | term_annot(term_rec_annot, term_rec_funct)

let term_rec_pair :=
  | term_rec_bind
  | term_pair(term_rec_pair, term_rec_bind)

let term_rec_both :=
  | term_rec_bind
  | term_both(term_rec_both, term_rec_bind)

let term_var ==
  | var = VAR;
    { st_var (mk $loc) ~var:(Name.make var) }
let term_forall(self, lower) ==
  | param = lower; ARROW; return = self;
    { st_forall (mk $loc) ~param ~return }
let term_lambda(self, lower) ==
  | param = lower; FAT_ARROW; return = self;
    { st_lambda (mk $loc) ~param ~return }
let term_apply(self, lower) ==
  | lambda = self; arg = lower;
    { st_apply (mk $loc) ~lambda ~arg }
let term_pair(self, lower) ==
  | left = lower; COMMA; right = self;
    { st_pair (mk $loc) ~left ~right }
let term_both(self, lower) ==
  | left = lower; AMPERSAND; right = self;
    { st_both (mk $loc) ~left ~right }
let term_bind(self, lower) ==
  | bound = lower; EQUAL; value = lower;
    { st_bind (mk $loc) ~bound ~value }
let term_semi(self, lower) ==
  | left = lower; SEMICOLON; right = self;
    { st_semi (mk $loc) ~left ~right }
let term_annot(self, lower) ==
  | value = lower; COLON; type_ = self;
    { st_annot (mk $loc) ~value ~type_ }
let term_parens(content) ==
  | LEFT_PARENS; content = content; RIGHT_PARENS;
    { st_parens (mk $loc) ~content }
let term_braces(content) ==
  | LEFT_BRACE; content = content; RIGHT_BRACE;
    { st_braces (mk $loc) ~content }
