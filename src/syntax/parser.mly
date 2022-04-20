%{
open Tree

let make_loc (loc_start, loc_end) =
  Location.{ loc_start; loc_end; loc_ghost = false }

let make loc desc = {
  s_loc = make_loc loc;
  s_desc = desc;
}

let make_apply ~lambda ~args =
  List.fold_left
    (fun lambda arg ->
      (* TODO: is this loc right? *)
      let loc_start = lambda.s_loc.loc_start in
      let loc_end = arg.s_loc.loc_end in
      make (loc_start, loc_end) (S_apply { lambda; arg })
    )
    lambda
    args

let make_bind_lambda loc ~bound ~params ~value ~body =
  let value =
    (* TODO: fold_right bad *)
    List.fold_right
      (fun param body ->
        (* TODO: is this loc right? *)
        let loc_start = param.s_loc.loc_start in
        let loc_end = body.s_loc.loc_end in
        make (loc_start, loc_end) (S_lambda { param; body })
      )
      params
      value in
  make loc (S_bind { bound; value = Some value; body })
%}
%token <string> IDENT
%token <string> NUMBER
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
  | annot
let term_lambda == lambda(term_bind)
let term_bind == bind(term_match)
let term_match := match_(term_match, apply)

let atom ==
  | field
  | simple_atom

let simple_atom ==
  | ident
  | number
  | struct_
  | parens

(* concrete syntax *)
let ident :=
  | ident = IDENT;
    { make $loc (S_ident ident) }

let number :=
  | number = NUMBER;
    { make $loc (S_number number) }

let lambda(lower) :=
  | lower
  | param = atom; ARROW; body = lambda(lower);
    { make $loc (S_lambda { param; body }) }

let atom_juxtaposition :=
  | first = atom; remaining = list(atom);
    { (first, remaining) }
let apply :=
  | (lambda, args) = atom_juxtaposition;
    { make_apply ~lambda ~args }

let bind(lower) :=
  | lower
  | bound = annot; SEMICOLON;
    body = option(bind(lower));
    { make $loc (S_bind { bound; value = None; body }) }
  | bound = annot; EQUAL;
    value = bind(lower); SEMICOLON;
    body = option(bind(lower));
    { make $loc (S_bind { bound; value = Some value; body }) }
  | (bound, params) = atom_juxtaposition; EQUAL;
    value = bind(lower); SEMICOLON;
    body = option(bind(lower));
    { make_bind_lambda $loc ~bound ~params ~value ~body }

let struct_ :=
  | LEFT_BRACE; term = option(term); RIGHT_BRACE;
    (* TODO: should loc include {} here? *)
    { make $loc (S_struct term) }

let field :=
  | struct_ = atom; DOT; field = simple_atom;
    { make $loc (S_field ({ struct_; field })) }

let match_(self, lower) ==
  | lower
  (* TODO: same syntax as lambda *)
  (* TODO: apply can be more general, maybe allow binds without match *)
  (* TODO: this body is not looking good *)
  | value = self; PIPE; pat = apply; ARROW; body = bind(lambda(lower));
    { make $loc (S_match { value; pat; body }) }

let annot :=
  (* TODO: value can be more general *)
  (* TODO: type_ can be more general *)
  | value = atom; COLON; type_ = lambda(atom);
    { make $loc (S_annot { value; type_ }) }
  
let parens ==
  | LEFT_PARENS; term = term; RIGHT_PARENS;
    { term }
