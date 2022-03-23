open Syntax

module Utils = struct
  module Location = struct
    include Location

    (* TODO: print *)
    let pp fmt _loc = Format.fprintf fmt "\"loc\""
  end

  type identifier = string [@@deriving show]

  type term = Syntax.term = {
    s_location : Location.t;
    s_description : term_description;
  }

  and term_description = Syntax.term_description =
    | S_variable of identifier
    | S_lambda of { parameter : term; body : term }
    | S_apply of { lambda : term; argument : term }
    | S_binding of { pattern : term; value : term; body : term option }
    | S_structure of term option
    | S_field of { structure : term; field : term }
    | S_match of { value : term; pattern : term; body : term }
    | S_constraint of { value : term; type_ : term }
  [@@deriving show { with_path = false }]
end

let make description =
  { s_location = Location.none; s_description = description }

let var name = make (S_variable name)
let lam param body = make (S_lambda { parameter = param; body })
let app lam arg = make (S_apply { lambda = lam; argument = arg })
let let_ pat value body = make (S_binding { pattern = pat; value; body })
let struct_ content = make (S_structure content)
let field struct_ field = make (S_field { structure = struct_; field })
let match_ value pat body = make (S_match { value; pattern = pat; body })
let constr value type_ = make (S_constraint { value; type_ })

module Simple = struct
  let variable = ("variable", "x", var "x")
  let lambda = ("lambda", "x -> x", lam (var "x") (var "x"))
  let apply = ("apply", "f x", app (var "f") (var "x"))

  (* TODO: semicolon *)
  let binding_none = ("binding_none", "x = y;", let_ (var "x") (var "y") None)

  let binding_body =
    ("binding_body", "x = y; z", let_ (var "x") (var "y") (Some (var "z")))

  let structure_empty = ("structure_empty", "{}", struct_ None)
  let structure_var = ("structure_var", "{ x }", struct_ (Some (var "x")))
  let field = ("field", "x.y", field (var "x") (var "y"))
  let match_ = ("match", "x | y -> z", match_ (var "x") (var "y") (var "z"))
  let constraint_ = ("constraint", "(x: int)", constr (var "x") (var "int"))

  let tests =
    [
      variable;
      lambda;
      apply;
      binding_none;
      binding_body;
      structure_empty;
      structure_var;
      field;
      match_;
      constraint_;
    ]
end

module Parens = struct
  let lambda_binding =
    ( "lambda_binding",
      "x -> y = x; y",
      lam (var "x") (let_ (var "y") (var "x") (Some (var "y"))) )

  let multiple_apply =
    ("multiple_apply", "f a b", app (app (var "f") (var "a")) (var "b"))

  let multiple_field =
    ("field_binding", "M.X.y", field (field (var "M") (var "X")) (var "y"))

  let field_binding =
    ( "field_binding",
      "M.X = y;",
      let_ (field (var "M") (var "X")) (var "y") None )

  let multiple_match =
    ( "multiple_match",
      "x | a -> y | b -> z",
      match_ (match_ (var "x") (var "a") (var "y")) (var "b") (var "z") )

  let constraint_on_structure =
    ( "constraint_on_structure",
      "{ M: Monad }",
      struct_ (Some (constr (var "M") (var "Monad"))) )
  (* TODO: problem is match on lambdas and bindings *)
  (* let match_arrow =
     ( "match_arrow",
       "x | a -> b -> a",
       match_ (var "x") (var "a") (lam (var "b") (var "a")) ) *)

  (* TODO: problem is bindings on lambdas and constraint *)
  (* let binding_constraint =
     ( "binding_constraint",
       "x: a -> a = y;",
       let_ (constr (var "x") (lam (var "a") (var "a"))) (var "y") None ) *)

  let tests =
    [
      lambda_binding;
      multiple_apply;
      multiple_field;
      field_binding;
      multiple_match;
      constraint_on_structure
      (* match_arrow; *)
      (* binding_constraint; *);
    ]
end

let term =
  Alcotest.testable Utils.pp_term (fun a b ->
      let a = Format.asprintf "%a" Utils.pp_term a in
      let b = Format.asprintf "%a" Utils.pp_term b in
      String.equal a b)

let test (name, code, expected) =
  let check () =
    let received = value_from_string code |> Option.get in
    Alcotest.(check term code expected received)
  in
  Alcotest.test_case name `Quick check

let simple = ("simple", List.map test Simple.tests)
let parens = ("parens", List.map test Parens.tests)
let () = Alcotest.run "Syntax" [ simple; parens ]
