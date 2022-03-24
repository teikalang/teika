%{
open Tree

let make_location (loc_start, loc_end) =
  Location.{ loc_start; loc_end; loc_ghost = false }

let make location description = {
  s_location = make_location location;
  s_description = description;
}

let make_apply ~lambda ~arguments =
  List.fold_left
    (fun lambda argument ->
      (* TODO: is this loc right? *)
      let loc_start = lambda.s_location.loc_start in
      let loc_end = argument.s_location.loc_end in
      make (loc_start, loc_end) (S_apply { lambda; argument })
    )
    lambda
    arguments

let make_binding loc ~pattern ~parameters ~value ~body =
  let value =
    (* TODO: fold_right bad *)
    List.fold_right
      (fun parameter body ->
        (* TODO: is this loc right? *)
        let loc_start = parameter.s_location.loc_start in
        let loc_end = body.s_location.loc_end in
        make (loc_start, loc_end) (S_lambda { parameter; body })
      )
      parameters
      value in
  make loc (S_binding { pattern; value; body })
%}
%token <string> IDENT

%token ARROW
%token EQUAL
%token COLON
%token SEMICOLON
%token DOT
%token PIPE
%token LEFT_BRACE
%token RIGHT_BRACE
(* TODO: {LEFT,RIGHT}_PARENTHESIS???? *)
%token LEFT_PARENS
%token RIGHT_PARENS

%token EOF

%start <Tree.term option> term_opt

%%

let term_opt :=
  | EOF;
    { None }
  | term = term; EOF;
    { Some term }

(* precedence on parens and braces *)
let term ==
  | term_lambda
  | constraint_
let term_lambda == lambda(term_binding)
let term_binding == binding(term_match)
let term_match := match_(term_match, apply)

let atom ==
  | ident
  | field
  | structure
  | parens

let simple_atom ==
  | ident
  | structure
  | parens

(* concrete syntax *)
let ident :=
  | ident = IDENT;
    { make $loc (S_variable ident) }

let lambda(lower) :=
  | lower
  | parameter = atom; ARROW; body = lambda(lower);
    { make $loc (S_lambda { parameter; body }) }

let atom_juxtaposition :=
  | first = atom; remaining = list(atom);
    { (first, remaining) }
let apply :=
  | (lambda, arguments) = atom_juxtaposition;
    { make_apply ~lambda ~arguments }

let binding(lower) :=
  | lower
  | pattern = constraint_; EQUAL;
    value = binding(lower); SEMICOLON;
    body = option(binding(lower));
    { make_binding $loc ~pattern ~parameters:[] ~value ~body }
  | (pattern, parameters) = atom_juxtaposition; EQUAL;
    value = binding(lower); SEMICOLON;
    body = option(binding(lower));
    { make_binding $loc ~pattern ~parameters ~value ~body }

let structure :=
  | LEFT_BRACE; term = option(term); RIGHT_BRACE;
    (* TODO: should loc include {} here? *)
    { make $loc (S_structure term) }

let field :=
  | structure = atom; DOT; field = simple_atom;
    { make $loc (S_field ({ structure; field })) }

let match_(self, lower) ==
  | lower
  (* TODO: same syntax as lambda *)
  (* TODO: apply can be more general, maybe allow bindings without match *)
  (* TODO: this body is not looking good *)
  | value = self; PIPE; pattern = apply; ARROW; body = binding(lambda(lower));
    { make $loc (S_match { value; pattern; body }) }

let constraint_ :=
  (* TODO: value can be more general *)
  (* TODO: type_ can be more general *)
  | value = atom; COLON; type_ = lambda(atom);
    { make $loc (S_constraint { value; type_ }) }
  
let parens ==
  | LEFT_PARENS; term = term; RIGHT_PARENS;
    { term }
