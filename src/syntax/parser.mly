%{
open Tree

let make_location (loc_start, loc_end) =
  Location.{ loc_start; loc_end; loc_ghost = false }

let make location description = {
  s_location = make_location location;
  s_description = description;
}

%}
%token <string> VARIABLE

%token ARROW
%token EQUAL
%token COLON
%token SEMICOLON
%token DOT
%token PIPE
(* TODO: {LEFT,RIGHT}_PARENTHESIS???? *)
%token LEFT_PARENS
%token RIGHT_PARENS
%token LEFT_BRACE
%token RIGHT_BRACE

%token EOF

%start <Tree.term option> value_opt

%%

let value_opt :=
  | EOF;
    { None }
  | term = value; EOF;
    { Some term }

(* WARNING: when changing this any of the rules,
    read the comment above it, in many cases it will be duplicated *)


(* all rules that never needs parens
   WARNING: check also at field_field *)
let atom ==
  | s_variable
  | s_field
  | s_structure
  | parens

(* all except constraint  *)
let value :=
  | s_lambda
  | s_apply
  | s_binding
  | s_match
  | atom

let parens :=
  | LEFT_PARENS; syntax = parens_content; RIGHT_PARENS;
    { syntax }
let parens_content ==
  | s_variable
  | s_lambda
  | s_apply
  | s_binding
  | s_structure
  | s_field
  | s_constraint

let s_variable ==
  | variable = VARIABLE;
    { make $loc (S_variable variable) }

let s_lambda ==
  | parameter = atom; ARROW; body = value;
    { make $loc (S_lambda { parameter; body }) }

let s_apply :=
  | lambda = apply_lambda; argument = atom;
    { make $loc (S_apply { lambda; argument }) }
let apply_lambda ==
  | s_apply
  | atom

let s_binding ==
  | pattern = atom; EQUAL; value = value; SEMICOLON;
    { make $loc (S_binding { pattern; value; body = None }) }
  | pattern = atom; EQUAL; value = value; SEMICOLON;
    body = value;
    { make $loc (S_binding { pattern; value; body = Some body }) }

let s_structure :=
  | LEFT_BRACE; RIGHT_BRACE;
    { make $loc (S_structure None) }
  | LEFT_BRACE; content = parens_content; RIGHT_BRACE;
    { make $loc (S_structure (Some content)) }

let s_field :=
  (* TODO: field could be value? *)
  | structure = atom; DOT; field = field_field;
    { make $loc (S_field ({ structure; field }) )}
let field_field ==
  | s_variable
  | s_structure
  | parens

let s_match :=
  | value = match_value; PIPE; pattern = atom; ARROW; body = atom;
    { make $loc (S_match { value; pattern; body }) }
let match_value ==
  | s_match
  | atom

let s_constraint ==
  | value = value; COLON; type_ = value;
    { make $loc (S_constraint { value; type_ }) }

