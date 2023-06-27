%{
open Stree

let wrap (loc_start, loc_end) term =
  let loc = Location.{ loc_start; loc_end; loc_ghost = false } in
  ST_loc { term; loc }

%}
%token <string> VAR (* x *)
%token FORALL (* -> *)
%token LAMBDA (* => *)
%token FIX (* @-> *)
%token SELF (* @=> *)
%token UNROLL (* @ *)
%token EXPAND (* %expand *)
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
  | term_self(term_rec_funct, term_rec_apply)
  | term_fix(term_rec_funct, term_rec_apply)

let term_rec_apply :=
  | term_atom
  | term_apply(term_rec_apply, term_atom)
  | term_expand(term_atom)

let term_atom :=
  | term_var
  | term_unroll(term_atom)
  | term_parens(term)

let term_var ==
  | var = VAR;
    { wrap $loc @@ ST_var { var = Name.make var } }
let term_forall(self, lower) ==
  | param = lower; FORALL; return = self;
    { wrap $loc @@ ST_forall { param; return } }
let term_lambda(self, lower) ==
  | param = lower; LAMBDA; return = self;
    { wrap $loc @@ ST_lambda { param; return } }
let term_self(self, lower) ==
  | bound = lower; SELF; body = self;
    { wrap $loc @@ ST_self { bound; body } }
let term_fix(self, lower) ==
  | bound = lower; FIX; body = self;
    { wrap $loc @@ ST_fix { bound; body } }
let term_unroll(lower) ==
  | UNROLL; term = lower;
    { wrap $loc @@ ST_unroll { term } }
let term_expand(lower) ==
  | EXPAND; term = lower;
    { wrap $loc @@ ST_expand { term } }
let term_apply(self, lower) ==
  | lambda = self; arg = lower;
    { wrap $loc @@ ST_apply { lambda; arg } }
let term_alias(self, lower) ==
  | bound = lower; ALIAS; value = lower; SEMICOLON; return = self;
  { wrap $loc @@ ST_alias { bound; value; return } }
let term_annot(self, lower) ==
  | term = lower; COLON; annot = self;
    { wrap $loc @@ ST_annot { term; annot } }
let term_parens(content) ==
  | LEFT_PARENS; term = content; RIGHT_PARENS;
    { wrap $loc @@ ST_parens { term } }
