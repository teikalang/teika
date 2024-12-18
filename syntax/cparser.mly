%{
open Utils
open Ctree

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
%token <string> STRING (* "abc" *)
%token <Z.t> NUMBER (* 123 *)
%token LEFT_PARENS (* ( *)
%token RIGHT_PARENS (* ) *)
%token LEFT_BRACE (* { *)
%token RIGHT_BRACE (* } *)
%token <string> EXTENSION (* %x *)

%token EOF

%start <Ctree.term option> term_opt
%%

let term_opt :=
  | EOF;
    { None }
  | term = term; EOF;
    { Some term }

let term := term_rec_pair

let term_rec_pair :=
  | term_semi_or_annot
  | term_pair(term_rec_pair, term_semi_or_annot)

let term_semi_or_annot :=
  | term_rec_annot
  | term_semi(term_rec_semi, term_rec_semi_bind)

let term_rec_semi :=
  | term_rec_funct
  | term_semi(term_rec_semi, term_rec_semi_bind)

let term_rec_semi_bind :=
  | term_rec_semi_annot
  | term_bind(term_rec_semi, term_rec_semi_annot)

let term_rec_semi_annot :=
  | term_rec_funct
  | term_annot(term_rec_semi_annot, term_rec_funct)

let term_rec_annot :=
  | term_rec_funct
  | term_annot(term_rec_annot, term_rec_funct)

let term_rec_funct :=
  | term_rec_apply
  | term_forall(term_rec_funct, term_rec_apply)
  | term_lambda(term_rec_funct, term_rec_apply)
  | term_both(term_rec_funct, term_rec_apply)

let term_rec_apply :=
  | term_atom
  | term_apply(term_rec_apply, term_atom)

let term_atom :=
  | term_var
  | term_extension
  | term_string
  | term_number
  | term_parens(term)
  | term_braces(term)

let term_forall(self, lower) ==
  | param = lower; ARROW; body = self;
    { ct_forall (mk $loc) ~param ~body }
let term_lambda(self, lower) ==
  | param = lower; FAT_ARROW; body = self;
    { ct_lambda (mk $loc) ~param ~body }
let term_apply(self, lower) ==
  | funct = self; arg = lower;
    { ct_apply (mk $loc) ~funct ~arg }
let term_pair(self, lower) ==
  | left = lower; COMMA; right = self;
    { ct_pair (mk $loc) ~left ~right }
let term_both(self, lower) ==
  | left = lower; AMPERSAND; right = self;
    { ct_both (mk $loc) ~left ~right }
let term_bind(self, lower) ==
  | bound = lower; EQUAL; value = lower;
    { ct_bind (mk $loc) ~bound ~value }
let term_semi(self, lower) ==
  | left = lower; SEMICOLON; right = self;
    { ct_semi (mk $loc) ~left ~right }
let term_annot(self, lower) ==
  | value = lower; COLON; annot = self;
    { ct_annot (mk $loc) ~value ~annot }
let term_var ==
  | var = VAR;
    { ct_var (mk $loc) ~var:(Name.make var) }
let term_extension ==
  | extension = EXTENSION;
    { ct_extension (mk $loc) ~extension:(Name.make extension) }
let term_string ==
  | literal = STRING;
    { ct_string (mk $loc) ~literal }
let term_number ==
  | literal = NUMBER;
    { ct_number (mk $loc) ~literal }
let term_parens(content) ==
  | LEFT_PARENS; content = content; RIGHT_PARENS;
    { ct_parens (mk $loc) ~content }
let term_braces(content) ==
  | LEFT_BRACE; content = content; RIGHT_BRACE;
    { ct_braces (mk $loc) ~content }
