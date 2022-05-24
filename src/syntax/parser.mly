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
%token FAT_ARROW
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

(* difference between term and type is just the precedence *)
let term_opt :=
  | EOF;
    { None }
  | term = term; EOF;
    { Some term }

(* precedence on parens and braces *)
let term ==
  | term_match
  | annot
let term_match :=
  | term_arrows_and_bind
  | match_(term_match, term_arrows_and_bind)
let term_arrows_and_bind :=
  | apply
  | bind(term_arrows_and_bind)
  | arrows(term_arrows_and_bind)

let atom :=
  | simple_atom
  | field(atom, simple_atom)

let simple_atom :=
  | ident
  | number
  | struct_
  | parens

let type_ == type_arrows
let type_arrows :=
  (* TODO: this could be more generic *)
  | apply
  | arrows(type_arrows)

(* concrete syntax *)
let ident ==
  | ident = IDENT;
    { make $loc (S_ident ident) }

let number ==
  | number = NUMBER;
    { make $loc (S_number number) }

let arrows(self) ==
  (* TODO: should the two arrows be always identical in precedence? *)
  | param = atom; ARROW; body = self;
    { make $loc (S_arrow { param; body }) }
  | param = atom; FAT_ARROW; body = self;
    { make $loc (S_lambda { param; body }) }

let atom_juxtaposition ==
  | first = atom; remaining = list(atom);
    { (first, remaining) }
let apply ==
  | (lambda, args) = atom_juxtaposition;
    { make_apply ~lambda ~args }

let bind(self) ==
  | bound = annot; SEMICOLON;
    body = option(self);
    { make $loc (S_bind { bound; value = None; body }) }
  | bound = annot; EQUAL;
    value = self; SEMICOLON;
    body = option(self);
    { make $loc (S_bind { bound; value = Some value; body }) }
  | (bound, params) = atom_juxtaposition; EQUAL;
    value = self; SEMICOLON;
    body = option(self);
    { make_bind_lambda $loc ~bound ~params ~value ~body }

let struct_ ==
  | LEFT_BRACE; term = option(term); RIGHT_BRACE;
    (* TODO: should loc include {} here? *)
    { make $loc (S_struct term) }

let field(self, lower) ==
  | struct_ = self; DOT; field = lower;
    { make $loc (S_field ({ struct_; field })) }

let match_(self, lower) ==
  (* TODO: same syntax as lambda *)
  (* TODO: apply can be more general, maybe allow binds without match *)
  (* TODO: this body is not looking good *)
  | value = self; PIPE; pat = apply; FAT_ARROW; body = lower;
    { make $loc (S_match { value; pat; body }) }


let annot ==
  (* TODO: value can be more general *)
  (* TODO: type_ can be more general *)
  | value = apply; COLON; type_ = type_;
    { make $loc (S_annot { value; type_ }) }
  
let parens ==
  | LEFT_PARENS; term = term; RIGHT_PARENS;
    { term }
