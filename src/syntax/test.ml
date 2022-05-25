open Syntax

module Utils = struct
  module Location = struct
    include Location

    (* TODO: print *)
    let pp fmt _loc = Format.fprintf fmt "\"loc\""
  end

  type identifier = string [@@deriving show]
  type number = string [@@deriving show]

  type term = Syntax.term = { s_loc : Location.t; s_desc : term_desc }

  and term_desc = Syntax.term_desc =
    | S_ident of identifier
    | S_number of number
    | S_arrow of { param : term; body : term }
    | S_lambda of { param : term; body : term }
    | S_apply of { lambda : term; arg : term }
    | S_bind of { bound : term; value : term option; body : term option }
    | S_struct of term option
    | S_field of { struct_ : term; field : term }
    | S_match of { value : term; pat : term; body : term }
    | S_asterisk
    | S_annot of { value : term; type_ : term }
  [@@deriving show { with_path = false }]
end

let make desc = { s_loc = Location.none; s_desc = desc }
let var name = make (S_ident name)
let number number = make (S_number number)
let ( @-> ) param body = make (S_arrow { param; body })
let ( @=> ) param body = make (S_lambda { param; body })
let app lambda arg = make (S_apply { lambda; arg })
let bind bound value body = make (S_bind { bound; value; body })
let struct_ content = make (S_struct content)
let field struct_ field = make (S_field { struct_; field })
let match_ value pat body = make (S_match { value; pat; body })
let asterisk = make S_asterisk
let annot value type_ = make (S_annot { value; type_ })

module Simple = struct
  let variable = ("variable", "x", var "x")
  let number = ("number", "123", number "123")
  let arrow = ("arrow", "Int -> Int", var "Int" @-> var "Int")
  let lambda = ("lambda", "x => x", var "x" @=> var "x")
  let apply = ("apply", "f x", app (var "f") (var "x"))

  (* TODO: semicolon *)
  let binding_no_value_no_type =
    ("binding_no_value_no_type", "x;", bind (var "x") None None)

  let binding_no_value_no_body =
    ( "binding_no_value_no_body",
      "x: Int;",
      bind (annot (var "x") (var "Int")) None None )

  let binding_no_value =
    ( "binding_no_value",
      "x: Int; z",
      bind (annot (var "x") (var "Int")) None (Some (var "z")) )

  let binding_no_body =
    ("binding_no_body", "x = y;", bind (var "x") (Some (var "y")) None)

  let binding_body =
    ( "binding_body",
      "x = y; z",
      bind (var "x") (Some (var "y")) (Some (var "z")) )

  let structure_empty = ("structure_empty", "{}", struct_ None)
  let structure_var = ("structure_var", "{ x }", struct_ (Some (var "x")))
  let field = ("field", "x.y", field (var "x") (var "y"))
  let match_ = ("match", "x | y => z", match_ (var "x") (var "y") (var "z"))
  let asterisk = ("asterisk", "*", asterisk)
  let annotation = ("annotation", "(x: Int)", annot (var "x") (var "Int"))

  let tests =
    [
      variable;
      number;
      arrow;
      lambda;
      apply;
      binding_no_value_no_type;
      binding_no_value_no_body;
      binding_no_value;
      binding_no_body;
      binding_body;
      structure_empty;
      structure_var;
      field;
      match_;
      asterisk;
      annotation;
    ]
end

module Parens = struct
  let arrow_binding =
    ( "arrow_binding",
      "x -> y = x; y",
      var "x" @-> bind (var "y") (Some (var "x")) (Some (var "y")) )

  let lambda_binding =
    ( "lambda_binding",
      "x => y = x; y",
      var "x" @=> bind (var "y") (Some (var "x")) (Some (var "y")) )

  let binding_arrow =
    ( "binding_arrow",
      "y = x -> x; y",
      bind (var "y") (Some (var "x" @-> var "x")) (Some (var "y")) )

  let binding_lambda =
    ( "binding_lambda",
      "y = x => x; y",
      bind (var "y") (Some (var "x" @=> var "x")) (Some (var "y")) )

  let multiple_apply =
    ("multiple_apply", "f a b", app (app (var "f") (var "a")) (var "b"))

  let multiple_field =
    ("field_binding", "M.X.y", field (field (var "M") (var "X")) (var "y"))

  let multiple_binding =
    ( "multiple_binding",
      "x = y; z = x; z",
      bind (var "x")
        (Some (var "y"))
        (Some (bind (var "z") (Some (var "x")) (Some (var "z")))) )

  let multiple_binding_annot =
    ( "multiple_binding_annot",
      "x: Int; y: Int;",
      bind
        (annot (var "x") (var "Int"))
        None
        (Some (bind (annot (var "y") (var "Int")) None None)) )

  let field_binding =
    ( "field_binding",
      "M.X = y;",
      bind (field (var "M") (var "X")) (Some (var "y")) None )

  let multiple_match =
    ( "multiple_match",
      "x | a => y | b => z",
      match_ (match_ (var "x") (var "a") (var "y")) (var "b") (var "z") )

  let annot_on_structure =
    ( "annot_on_structure",
      "{ M: Monad }",
      struct_ (Some (annot (var "M") (var "Monad"))) )

  let sequence_on_structure =
    ( "sequence_on_structure",
      "{ print 1; print 2; }",
      struct_
        (Some
           (bind
              (app (var "print") (number "1"))
              None
              (Some (bind (app (var "print") (number "2")) None None)))) )

  let sequence_with_binding =
    ( "sequence_with_binding",
      "print y; x = y; print x",
      bind
        (app (var "print") (var "y"))
        None
        (Some
           (bind (var "x")
              (Some (var "y"))
              (Some (app (var "print") (var "x"))))) )

  let binding_then_sequence =
    ( "binding_then_sequence",
      "print y; x = y; print x;",
      bind
        (app (var "print") (var "y"))
        None
        (Some
           (bind (var "x")
              (Some (var "y"))
              (Some (bind (app (var "print") (var "x")) None None)))) )

  let match_arrow_body =
    ( "match_arrow",
      "x | a => b -> a",
      match_ (var "x") (var "a") (var "b" @-> var "a") )

  let match_lambda_body =
    ( "match_lambda",
      "x | a => b => a",
      match_ (var "x") (var "a") (var "b" @=> var "a") )

  let match_bindbody =
    ( "match_bindbody",
      "x | a => b = a; b",
      match_ (var "x") (var "a")
        (bind (var "b") (Some (var "a")) (Some (var "b"))) )

  let annot_on_binding =
    ( "annot_on_binding",
      "x: a -> a = y;",
      bind (annot (var "x") (var "a" @-> var "a")) (Some (var "y")) None )

  let apply_annot =
    ( "apply_annot",
      "(f a: T A)",
      annot (app (var "f") (var "a")) (app (var "T") (var "A")) )

  let arrow_type_annot =
    ( "arrow_type_annot",
      "(f a: Int -> T A)",
      annot (app (var "f") (var "a")) (var "Int" @-> app (var "T") (var "A")) )

  let arrow_asterisk = ("arrow_asterisk", "* -> *", asterisk @-> asterisk)

  (* TODO: this should definitely work *)
  let _tests = [ binding_then_sequence ]

  let tests =
    [
      arrow_binding;
      lambda_binding;
      binding_arrow;
      binding_lambda;
      multiple_apply;
      multiple_field;
      multiple_binding;
      multiple_binding_annot;
      field_binding;
      multiple_match;
      annot_on_structure;
      sequence_on_structure;
      sequence_with_binding;
      match_arrow_body;
      match_lambda_body;
      match_bindbody;
      annot_on_binding;
      apply_annot;
      arrow_type_annot;
      arrow_asterisk;
    ]
end

module Sugar = struct
  let binding_lambda =
    ( "binding_lambda",
      "f a b = x;",
      bind (var "f") (Some (var "a" @=> var "b" @=> var "x")) None )

  let tests = [ binding_lambda ]
end

let term =
  Alcotest.testable Utils.pp_term (fun a b ->
      let a = Format.asprintf "%a" Utils.pp_term a in
      let b = Format.asprintf "%a" Utils.pp_term b in
      String.equal a b)

let test (name, code, expected) =
  let check () =
    let received =
      match value_from_string code with
      | Some received -> received
      | None -> failwith ("string likely empty: " ^ name)
    in
    Alcotest.(check term code expected received)
  in
  Alcotest.test_case name `Quick check

let simple = ("simple", List.map test Simple.tests)
let parens = ("parens", List.map test Parens.tests)
let sugar = ("sugar", List.map test Sugar.tests)
let () = Alcotest.run "Syntax" [ simple; parens; sugar ]
