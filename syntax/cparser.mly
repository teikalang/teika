%{
open Ctree

let mk (loc_start, loc_end) =
  Location.{ loc_start; loc_end; loc_ghost = false }

%}
%token UNDERSCORE (* _ *)
%token <string> VAR (* x *)
%token COLON (* : *)
%token EQUAL (* = *)
%token COMMA (* , *)
%token AMPERSAND (* & *)
%token PIPE (* | *)
%token SEMICOLON (* ; *)
%token <string> STRING (* "abc" *)
%token <Z.t> NUMBER (* 123 *)
%token LEFT_PARENS (* ( *)
%token RIGHT_PARENS (* ) *)
%token LEFT_BRACKET (* [ *)
%token RIGHT_BRACKET (* ] *)
%token LEFT_BRACE (* { *)
%token RIGHT_BRACE (* } *)

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
  | term_rec_union_or_inter
  | term_pair(term_rec_pair, term_rec_union_or_inter)

let term_rec_union_or_inter :=
  | term_rec_semi_or_bind_or_annot
  | term_inter(term_rec_union_or_inter, term_rec_semi_or_bind_or_annot)
  | term_union(term_rec_union_or_inter, term_rec_semi_or_bind_or_annot)

let term_rec_semi_or_bind_or_annot :=
  | term_rec_apply
  | term_bind_or_annot
  | term_semi(term_rec_semi_or_bind_or_annot, term_bind_or_annot)

let term_bind_or_annot ==
  | term_bind(term_rec_semi, term_rec_annot)
  | term_annot(term_rec_annot, term_rec_apply)

let term_rec_semi := 
  | term_rec_apply
  | term_semi(term_rec_semi, term_bind_or_annot)

let term_rec_annot :=
  | term_rec_apply
  | term_annot(term_rec_annot, term_rec_apply)

let term_rec_apply :=
  | term_atom
  | term_apply(term_rec_apply, term_atom)

let term_atom :=
  | term_hole
  | term_var
  | term_string
  | term_number
  | term_parens(term_opt)
  | term_brackets(term_opt)
  | term_braces(term_opt)

let term_apply(self, lower) ==
  | funct = self; arg = lower;
    { ct_apply (mk $loc) ~funct ~arg }
let term_pair(self, lower) ==
  | left = lower; COMMA; right = self;
    { ct_pair (mk $loc) ~left ~right }
let term_inter(self, lower) ==
  | left = lower; AMPERSAND; right = self;
    { ct_inter (mk $loc) ~left ~right }
let term_union(self, lower) ==
  | left = lower; PIPE; right = self;
    { ct_union (mk $loc) ~left ~right }
let term_bind(self, lower) ==
  | bound = lower; EQUAL; value = self;
    { ct_bind (mk $loc) ~bound ~value }
let term_semi(self, lower) ==
  | left = lower; SEMICOLON; right = self;
    { ct_semi (mk $loc) ~left ~right }
let term_annot(self, lower) ==
  | value = lower; COLON; annot = self;
    { ct_annot (mk $loc) ~value ~annot }
let term_hole ==
  | UNDERSCORE;
    { ct_hole (mk $loc) }
let term_var ==
  | var = VAR;
    { ct_var (mk $loc) ~var:(Name.make var) }
let term_string ==
  | literal = STRING;
    { ct_string (mk $loc) ~literal }
let term_number ==
  | literal = NUMBER;
    { ct_number (mk $loc) ~literal }
let term_parens(content) ==
  | LEFT_PARENS; content = content; RIGHT_PARENS;
    { ct_parens (mk $loc) ~content }
let term_brackets(content) ==
  | LEFT_BRACKET; content = content; RIGHT_BRACKET;
    { ct_brackets (mk $loc) ~content }
let term_braces(content) ==
  | LEFT_BRACE; content = content; RIGHT_BRACE;
    { ct_braces (mk $loc) ~content }
