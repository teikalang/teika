%{
open Ctree

let mk (loc_start, loc_end) =
  Location.{ loc_start; loc_end; loc_ghost = false }

%}
%token UNDERSCORE (* _ *)
%token <string> VAR (* x *)
%token SELF (* self *)
%token FIX (* fix *)
%token COLON (* : *)
%token EQUAL (* = *)
%token ARROW (* -> *)
%token FAT_ARROW (* => *)
%token COMMA (* , *)
%token AMPERSAND (* & *)
%token PIPE (* | *)
%token SEMICOLON (* ; *)
%token DOT_UNFOLD (* .@ *)
%token DOT_FST (* .0 *)
%token DOT_SND (* .1 *)
%token <string> STRING (* "abc" *)
%token <Z.t> NUMBER (* 123 *)
%token LEFT_PARENS (* ( *)
%token RIGHT_PARENS (* ) *)
%token LEFT_BRACKET (* [ *)
%token RIGHT_BRACKET (* ] *)
%token LEFT_BRACE (* { *)
%token RIGHT_BRACE (* } *)

%token EOF

%right ARROW
%right COLON

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
  | left = term_rec_union_or_inter; COMMA; right = term_rec_pair;
    { ct_pair (mk $loc) ~left ~right }

let term_rec_union_or_inter :=
  | term_rec_sequence
  | left = term_rec_sequence; AMPERSAND; right = term_rec_union_or_inter;
    { ct_inter (mk $loc) ~left ~right }
  | left = term_rec_sequence; PIPE; right = term_rec_union_or_inter;
    { ct_union (mk $loc) ~left ~right }

let term_rec_sequence :=
  | term_rec_binding
  | left = term_rec_binding; SEMICOLON; right = term_rec_sequence;
    { ct_semi (mk $loc) ~left ~right }

let term_rec_binding :=
  | term_rec_annot
  | bound = term_rec_annot; EQUAL; value = term_rec_binding;
    { ct_bind (mk $loc) ~bound ~value }

let term_rec_annot :=
  | value = term_rec_arrow; %prec ARROW
    { value }
  | value = term_rec_arrow; COLON; annot = term_rec_annot;
    { ct_annot (mk $loc) ~value ~annot }

let term_rec_arrow :=
  | term_rec_apply
  | bound = term_rec_apply; ARROW; body = term_rec_annot;
    { ct_pi (mk $loc) ~bound ~body }
  | bound = term_rec_apply; FAT_ARROW; body = term_rec_annot;
    { ct_lambda (mk $loc) ~bound ~body }
  | SELF; LEFT_PARENS; bound = term; RIGHT_PARENS; ARROW;
    body = term_rec_annot;
    { ct_self (mk $loc) ~bound ~body }
  | FIX; LEFT_PARENS; bound = term; RIGHT_PARENS; FAT_ARROW;
    body = term_rec_annot;
    { ct_fix (mk $loc) ~bound ~body }
  | FIX; LEFT_PARENS; bound = term; RIGHT_PARENS; COLON;
    annot = term_rec_apply; FAT_ARROW; body = term_rec_annot;
    { ct_fix (mk $loc)
        ~bound:(ct_annot (mk $loc) ~value:bound ~annot)
        ~body }

let term_rec_apply :=
  | term_postfix
  | funct = term_rec_apply; arg = term_postfix;
    { ct_apply (mk $loc) ~funct ~arg }

let term_postfix :=
  | term_atom
  | fix = term_postfix; DOT_UNFOLD;
    { ct_unfold (mk $loc) ~fix }
  | pair = term_postfix; DOT_FST;
    { ct_fst (mk $loc) ~pair }
  | pair = term_postfix; DOT_SND;
    { ct_snd (mk $loc) ~pair }

let term_atom :=
  | UNDERSCORE;
    { ct_hole (mk $loc) }
  | var = VAR;
    { ct_var (mk $loc) ~var:(Name.make var) }
  | literal = STRING;
    { ct_string (mk $loc) ~literal }
  | literal = NUMBER;
    { ct_number (mk $loc) ~literal }
  | LEFT_PARENS; RIGHT_PARENS;
    { ct_unit (mk $loc) ~delimiter:Parens }
  | LEFT_PARENS; content = term; RIGHT_PARENS;
    { ct_parens (mk $loc) ~content }
  | LEFT_BRACKET; RIGHT_BRACKET;
    { ct_unit (mk $loc) ~delimiter:Brackets }
  | LEFT_BRACKET; content = term; RIGHT_BRACKET;
    { ct_brackets (mk $loc) ~content }
  | LEFT_BRACE; content = term; RIGHT_BRACE;
    { ct_braces (mk $loc) ~content }
