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

(* WARNING: when changing this, think about the relationship below:
  - parens_content: what goes inside of a parens, can be anything.
    - parens
  - delimited: all syntax that never needs parens.
    - lambda, forall, apply, let
  - value: for top-level, forall / lambda body.
    - lambda, forall, let, constraint, structure
  - type_: for constraint type_
  - binding
  - , type_ and parameter, pattern *)
let parens_content ==
  | s_variable
  | s_lambda
  | s_apply
  | s_forall
  | s_binding
  | s_structure
  // | field
  | s_constraint

let delimited_ ==
  | s_variable
  (* TODO: should f = A.X -> 1 and A.X = 1 be allowed? *)
  // | field
  | s_structure
  | parens

(* all except constraint *)
let value :=
  | s_lambda
  | s_apply
  | s_forall
  | s_binding
  | delimited_

let parens :=
  | LEFT_PARENS; syntax = parens_content; RIGHT_PARENS;
    { syntax }

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
  | parameter = delimited_; DOT; body = value;
    { make $loc (S_forall { parameter; body }) }

let s_binding ==
  | pattern = binding_pattern; EQUAL; value = value; SEMICOLON;
    { make $loc (S_binding { pattern; value; body = None }) }
  | pattern = binding_pattern; EQUAL; value = value; SEMICOLON;
    body = value;
    { make $loc (S_binding { pattern; value; body = Some body }) }
let binding_pattern ==
  | s_constraint
  | delimited_

let s_constraint ==
  | value = delimited_; COLON; type_ = delimited_;
    { make $loc (S_constraint { value; type_ }) }

let s_structure ==
  | LEFT_BRACE; RIGHT_BRACE;
    { make $loc (S_structure None) }
  | LEFT_BRACE; content = value; RIGHT_BRACE;
    { make $loc (S_structure (Some content)) }
