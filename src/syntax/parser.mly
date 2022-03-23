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
%token FORALL_DOT
%token FIELD_DOT
%token PIPE
(* TODO: {LEFT,RIGHT}_PARENTHESIS???? *)
%token LEFT_PARENS
%token RIGHT_PARENS
%token LEFT_BRACE
%token RIGHT_BRACE

%token EOF

%start <Tree.syntax option> value_opt

%%

let value_opt :=
  | EOF;
    { None }
  | syntax = value; EOF;
    { Some syntax }

(* WARNING: when changing this any of the rules,
    read the comment above it, in many cases it will be duplicated *)


(* all rules that never needs parens
   WARNING: check also at field_field *)
let delimited_ ==
  | s_variable
  | s_field
  | s_structure
  | parens

(* all except constraint  *)
let value :=
  | s_lambda
  | s_apply
  | s_forall
  | s_binding
  | s_match
  | delimited_

let parens :=
  | LEFT_PARENS; syntax = parens_content; RIGHT_PARENS;
    { syntax }
let parens_content ==
  | s_variable
  | s_lambda
  | s_apply
  | s_forall
  | s_binding
  | s_structure
  | s_field
  | s_constraint

let s_variable ==
  | variable = VARIABLE;
    { make $loc (S_variable variable) }

let s_lambda ==
  | parameter = delimited_; ARROW; body = value;
    { make $loc (S_lambda { parameter; body }) }

let s_apply :=
  | lambda = apply_lambda; argument = delimited_;
    { make $loc (S_apply { lambda; argument }) }
let apply_lambda ==
  | s_apply
  | delimited_

let s_forall ==
  | parameter = delimited_; FORALL_DOT; body = value;
    { make $loc (S_forall { parameter; body }) }

let s_binding ==
  | pattern = delimited_; EQUAL; value = value; SEMICOLON;
    { make $loc (S_binding { pattern; value; body = None }) }
  | pattern = delimited_; EQUAL; value = value; SEMICOLON;
    body = value;
    { make $loc (S_binding { pattern; value; body = Some body }) }

let s_structure ==
  | LEFT_BRACE; RIGHT_BRACE;
    { make $loc (S_structure None) }
  | LEFT_BRACE; content = value; RIGHT_BRACE;
    { make $loc (S_structure (Some content)) }

let s_field :=
  (* TODO: field could be value? *)
  | structure = delimited_; FIELD_DOT; field = field_field;
    { make $loc (S_field ({ structure; field }) )}
let field_field ==
  | s_variable
  | s_structure
  | parens

let s_match :=
  | value = match_value; PIPE; pattern = delimited_; ARROW; body = delimited_;
    { make $loc (S_match { value; pattern; body }) }
let match_value ==
  | s_match
  | delimited_

let s_constraint ==
  | value = value; COLON; type_ = value;
    { make $loc (S_constraint { value; type_ }) }

