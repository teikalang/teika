%{
open Stree

let wrap (loc_start, loc_end) term =
  let loc = Location.{ loc_start; loc_end; loc_ghost = false } in
  ST_loc { term; loc }

%}
%token <string> VAR (* x *)
%token ARROW (* -> *)
%token FAT_ARROW (* => *)
%token ALIAS (* === *)
%token COLON (* : *)
%token SEMICOLON (* ; *)
%token LEFT_PARENS (* ( *)
%token RIGHT_PARENS (* ) *)

%token EOF

%start <Stree.term option> term_opt

%%

let term_opt :=
  | EOF;
    { None }
  | term = term; EOF;
    { Some term }


let term := term_rec_annot

let term_rec_annot :=
  | term_rec_alias
  | term_annot(term_rec_annot, term_rec_alias)

let term_rec_alias :=
  | term_rec_funct
  | term_alias(term_rec_alias, term_rec_funct)

let term_rec_funct :=
  | term_rec_apply
  | term_forall(term_rec_funct, term_rec_apply)
  | term_lambda(term_rec_funct, term_rec_apply)

let term_rec_apply :=
  | term_atom
  | term_apply(term_rec_apply, term_atom)

let term_atom :=
  | term_var
  | term_parens(term)

let term_var ==
  | var = VAR;
    { wrap $loc @@ ST_var { var = Name.make var } }
let term_forall(self, lower) ==
  | param = lower; ARROW; return = self;
    { wrap $loc @@ ST_forall { param; return } }
let term_lambda(self, lower) ==
  | param = lower; FAT_ARROW; return = self;
    { wrap $loc @@ ST_lambda { param; return } }
let term_apply(self, lower) ==
  | lambda = self; arg = lower;
    { wrap $loc @@ ST_apply { lambda; arg } }
let term_alias(self, lower) ==
  | bound = lower; ALIAS; value = lower; return = self;
  { wrap $loc @@ ST_alias { bound; value; return } }
let term_annot(self, lower) ==
  | term = lower; COLON; annot = self;
    { wrap $loc @@ ST_annot { term; annot } }
let term_parens(content) ==
  | LEFT_PARENS; term = term; RIGHT_PARENS;
    { wrap $loc @@ ST_parens { term } }
