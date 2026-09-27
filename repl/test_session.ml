open Repl_core
module C = Typer.Typed_tree

let evaluate source =
  match Session.evaluate source with
  | Some result -> result
  | None -> failwith ("expected a term: " ^ source)

let check_result source (result : Session.result) ~type_ ~value =
  let actual_type = Format.asprintf "%a" Printer.pp result.type_ in
  let actual_value = Format.asprintf "%a" Printer.pp result.value in
  if actual_type <> type_ || actual_value <> value then
    failwith (Format.asprintf "%S: got %s : %s" source actual_value actual_type)

let check source ~type_ ~value =
  check_result source (evaluate source) ~type_ ~value

let expect_rejection source prefix action =
  match action () with
  | _ -> failwith ("expected rejection: " ^ source)
  | exception (Typer.Error (loc, message) | Lparser.Error (loc, message)) ->
      if loc.loc_ghost || not (String.starts_with ~prefix message) then
        failwith ("unexpected diagnostic: " ^ message)
  | exception Failure message ->
      if not (String.starts_with ~prefix message) then failwith message
  | exception Lexer.Error (_, message) ->
      if not (String.starts_with ~prefix message) then failwith message

let reject source prefix =
  expect_rejection source prefix (fun () -> evaluate source)

let submit session source =
  match Session.submit session source with
  | session, Some result -> (session, result)
  | _, None -> failwith ("expected a result: " ^ source)

let check_line session source ~type_ ~value =
  let session, result = submit session source in
  check_result source result ~type_ ~value;
  session

let reject_line session source prefix =
  expect_rejection source prefix (fun () -> submit session source)

let test_identity_type () =
  (* Polymorphic identity: forall A. A -> A. *)
  let signature = "id(A : Type)(x : A)" in
  let type_ = "(self id. (Pi A : (self A. Type). (Pi x : (self x. A). A)))" in
  check (signature ^ " : A") ~type_:"Type" ~value:type_;
  check (signature ^ " = x") ~type_ ~value:"(fix id. (lambda A. (lambda x. x)))";
  check
    ("(" ^ signature ^ " = x)(Type)(t : Type)")
    ~type_:"Type" ~value:"(self t. Type)";
  reject (signature ^ " : A = A") "unify: type clash"

let test_constant_type () =
  (* The K combinator: forall A B. A -> B -> A. *)
  let signature = "const(A : Type)(B : Type)(x : A)(y : B)" in
  let type_ =
    "(self const. (Pi A : (self A. Type). (Pi B : (self B. Type). (Pi x : \
     (self x. A). (Pi y : (self y. B). A)))))"
  in
  check (signature ^ " : A") ~type_:"Type" ~value:type_;
  check (signature ^ " = x") ~type_
    ~value:"(fix const. (lambda A. (lambda B. (lambda x. (lambda y. x)))))";
  (* Distinct values expose accidentally returning the second argument. *)
  check
    ("(" ^ signature ^ " = x)(Type)(Type)(Type)(t : Type)")
    ~type_:"Type" ~value:"Type";
  reject (signature ^ " : A = y") "unify: type clash"

let test_composition_type () =
  (* This nested type previously exceeded a ten-second check because binder
     freshening repeated substitution exponentially. *)
  check
    "compose(A : Type)(B : Type)(C : Type)(f : (h(y : B) : C))(g : (k(x : A) : \
     B))(x : A) : C"
    ~type_:"Type"
    ~value:
      "(self compose. (Pi A : (self A. Type). (Pi B : (self B. Type). (Pi C : \
       (self C. Type). (Pi f : (self f. (self h. (Pi y : (self y. B). C))). \
       (Pi g : (self g. (self k. (Pi x : (self x. A). B))). (Pi x : (self x. \
       A). C)))))))"

let test_let () =
  check "x = Type; x" ~type_:"Type" ~value:"Type";
  check "x : Type = Type; x" ~type_:"Type" ~value:"Type";
  check "x = Type; y = x; y" ~type_:"Type" ~value:"Type";
  check "unused = Type; Type" ~type_:"Type" ~value:"Type";
  check "Type = Type; Type" ~type_:"Type" ~value:"Type";
  check "x = Type; x = x; x" ~type_:"Type" ~value:"Type";
  check "x = Type; x = (t : Type); x" ~type_:"Type" ~value:"(self t. Type)";
  check "x = Type; saved(z : Type) = x; x = (t : Type); saved(Type)"
    ~type_:"Type" ~value:"Type";
  check "A = Type; id(x : A) = x; id(Type)" ~type_:"Type" ~value:"Type";
  check "id(A : Type)(x : A) : A = x; id(Type)(t : Type)" ~type_:"Type"
    ~value:"(self t. Type)";
  check "id(A : Type)(x : A) = x; id"
    ~type_:"(Pi A : (self A. Type). (Pi x : (self x. A). A))"
    ~value:"(lambda A. (lambda x. x))";
  check "(f(A : Type)(x : A) = T = A; y : T = x; y)(Type)(Type)" ~type_:"Type"
    ~value:"Type";
  check "x = (y = Type; y); x" ~type_:"Type" ~value:"Type";
  check "f = (loop(x : Type) : Type = loop(x)); f"
    ~type_:"(self loop. (Pi x : (self x. Type). Type))"
    ~value:"(fix loop. (lambda x. (loop x)))";
  reject "x = y; y = Type; x" "unbound variable: y";
  reject "x = x; x" "unbound variable: x";
  reject "f(x : Type) : Type = f(x); f(Type)" "unbound variable: f";
  reject "id(A : Type)(x : A) = x; A" "unbound variable: A";
  reject "x = (y = Type; y); y" "unbound variable: y";
  reject "unused = Type(Type); Type" "unify: type clash";
  reject "id(A : Type)(x : A) : A = A; Type" "unify: type clash";
  reject "x : Type; x = Type; x" "check_block: declarations";
  reject "x = Type; y : Type; y = Type; y" "check_block: declarations";
  reject "(x, y) = Type; x" "infer_let: pairs";
  reject "[x] = Type; x" "infer_let: boxes";
  reject "() = Type; Type" "infer_let: unit"

let false_definition = "(False = (f(C(f : False) : Type) : C(f)))"

let false_value =
  "(fix False. (self f. (Pi C : (self C. (Pi f1 : (self f1. False). Type)). (C \
   f))))"

let test_recursive_type () =
  check false_definition ~type_:"(self False. Type)" ~value:false_value;
  check "(False : Type = (f(C(f : False) : Type) : C(f)))"
    ~type_:"(self False. Type)" ~value:false_value;
  check "(Empty(A : Type) : Type = (e(P(x : Empty(A)) : Type) : P(e)))"
    ~type_:"(self Empty. (Pi A : (self A. Type). Type))"
    ~value:
      "(fix Empty. (lambda A. (self e. (Pi P : (self P. (Pi x : (self x. \
       (Empty A)). Type)). (P e)))))";
  let renamed = evaluate "(Empty = (e(P(x : Empty) : Type) : P(e)))" in
  C.unify (evaluate false_definition).value renamed.value;
  (* A higher-order parameter's own arguments stay inside that parameter. *)
  check "f(C(x : Type) : Type) : C(Type)" ~type_:"Type"
    ~value:
      "(self f. (Pi C : (self C. (Pi x : (self x. Type). Type)). (C Type)))";
  reject "f(C(x : Type) : Type) : x" "unbound variable: x";
  reject "use(C(x : Type) : Type) = x; Type" "unbound variable: x";
  reject "f(C(f : Type) : Type) : C(f)" "unify: type clash";
  reject
    ("False = " ^ false_definition ^ "; bad : False = Type; bad")
    "unify: type clash";
  let session =
    check_line Session.empty
      ("False = " ^ false_definition)
      ~type_:"(self False. Type)" ~value:false_value
  in
  let session =
    check_line session "False" ~type_:"(self False. Type)" ~value:false_value
  in
  (* The eliminator must unfold the saved recursive type when applying f. *)
  let source = "absurd(f : False)(A : Type) : A = f(C(x : False) = A)" in
  let session, result = submit session source in
  let type_ =
    "(Pi f : (self f. (fix False. (self f1. (Pi C : (self C. (Pi f2 : (self \
     f2. False). Type)). (C f1))))). (Pi A : (self A. Type). A))"
  in
  let value = "(lambda f. (lambda A. (f (fix C. (lambda x. A)))))" in
  (* Bound-name freshening inside False avoids the eliminator's outer f. *)
  check_result source result ~type_ ~value;
  ignore (check_line session "absurd" ~type_ ~value);
  reject_line session "bad : False = Type" "unify: type clash";
  ignore
    (check_line session "False" ~type_:"(self False. Type)" ~value:false_value)

let test_persistent_session () =
  let session =
    check_line Session.empty "A = Type; // type alias" ~type_:"Type"
      ~value:"Type"
  in
  let session =
    check_line session "id(x : A) = x;" ~type_:"(Pi x : (self x. Type). Type)"
      ~value:"(lambda x. x)"
  in
  let session = check_line session "id(A)" ~type_:"Type" ~value:"Type" in
  let session = check_line session "saved = A" ~type_:"Type" ~value:"Type" in
  let session =
    check_line session "A = (t : Type)" ~type_:"Type" ~value:"(self t. Type)"
  in
  let session = check_line session "saved" ~type_:"Type" ~value:"Type" in
  let session =
    check_line session "A = A" ~type_:"Type" ~value:"(self t. Type)"
  in
  let session =
    check_line session "id(A)" ~type_:"Type" ~value:"(self t. Type)"
  in
  let same, output = Session.submit session " // comment" in
  if same != session || Option.is_some output then
    failwith "empty line changed session";
  let session =
    check_line session "local = Type; local" ~type_:"Type" ~value:"Type"
  in
  reject_line session "local" "unbound variable: local";
  let session =
    check_line session "(named(x : Type) = x)"
      ~type_:"(self named. (Pi x : (self x. Type). Type))"
      ~value:"(fix named. (lambda x. x))"
  in
  reject_line session "named" "unbound variable: named";
  let session =
    check_line session "pending : Type" ~type_:"Type"
      ~value:"(self pending. Type)"
  in
  reject_line session "pending" "unbound variable: pending";
  reject_line session "future = missing" "unbound variable: missing";
  reject_line session "future" "unbound variable: future";
  reject_line session "self = self" "unbound variable: self";
  reject_line session "x : Type; x = Type; x" "check_block: declarations";
  reject_line session "broken = (" "syntax error";
  reject_line session "[boxed] = Type" "infer_let: boxes";
  let session =
    check_line session "Type = A" ~type_:"Type" ~value:"(self t. Type)"
  in
  ignore (check_line session "Type" ~type_:"Type" ~value:"(self t. Type)");
  reject_line Session.empty "A" "unbound variable: A";
  ignore (check_line Session.empty "Type" ~type_:"Type" ~value:"Type")

let test_session_isolation () =
  let session =
    check_line Session.empty "choose(x) = Type" ~type_:"(Pi x : (self ?). Type)"
      ~value:"(lambda x. Type)"
  in
  let session =
    check_line session "saved = choose" ~type_:"(Pi x : (self ?). Type)"
      ~value:"(lambda x. Type)"
  in
  (* Checking this call constrains choose's parameter before its result is
     incorrectly applied. Those partial solutions must not escape failure. *)
  reject_line session "bad = choose(Type)(Type)" "unify: type clash";
  ignore
    (check_line session "choose" ~type_:"(Pi x : (self ?). Type)"
       ~value:"(lambda x. Type)");
  reject_line session "bad" "unbound variable: bad";
  let updated = check_line session "choose(Type)" ~type_:"Type" ~value:"Type" in
  ignore
    (check_line updated "choose" ~type_:"(Pi x : (self x. Type). Type)"
       ~value:"(lambda x. Type)");
  ignore
    (check_line updated "saved" ~type_:"(Pi x : (self x. Type). Type)"
       ~value:"(lambda x. Type)");
  (* Session values are persistent even after a successful derived command. *)
  ignore
    (check_line session "choose" ~type_:"(Pi x : (self ?). Type)"
       ~value:"(lambda x. Type)")

let test_locations () =
  let result = evaluate "(id(A : Type)(x : A) = x)(Type)(Type)" in
  let rec visit seen term =
    let term = C.t_repr term in
    if List.exists (C.t_raw_same term) seen then seen
    else
      let loc = C.t_loc term in
      if
        loc.loc_ghost || loc.loc_start.pos_cnum < 0
        || loc.loc_end.pos_cnum <= loc.loc_start.pos_cnum
      then failwith "generated term lost its source location";
      let seen = term :: seen in
      match C.t_head term with
      | C.T_flex | C.T_type -> seen
      | C.T_var { var = C.Var { to_; _ } } -> (
          match to_ with None -> seen | Some value -> visit seen value)
      | C.T_lambda { body; _ } -> visit seen body
      | C.T_pi { param; body } -> visit (visit seen param) body
      | C.T_apply { funct; arg } -> visit (visit seen funct) arg
      | C.T_self { body } | C.T_fix { body } -> visit seen body
      | C.T_let { arg; body; _ } -> visit (visit seen arg) body
  in
  ignore (visit [] result.type_);
  ignore (visit [] result.value);
  let result = evaluate "id(A : Type)(x : A) = x" in
  ignore (visit [] result.type_);
  ignore (visit [] result.value);
  let result = evaluate "A = Type; id(x : A) = x; id" in
  ignore (visit [] result.type_);
  ignore (visit [] result.value);
  let session, result = submit Session.empty ("False = " ^ false_definition) in
  ignore (visit [] result.type_);
  ignore (visit [] result.value);
  let _, result = submit session "False" in
  ignore (visit [] result.type_);
  ignore (visit [] result.value);
  match evaluate "   missing" with
  | _ -> failwith "expected unbound variable"
  | exception Typer.Error (loc, _) ->
      if loc.loc_start.pos_cnum <> 3 then failwith "incorrect error location"

let () =
  test_identity_type ();
  test_constant_type ();
  test_composition_type ();
  test_let ();
  test_recursive_type ();
  test_persistent_session ();
  test_session_isolation ();
  if Session.evaluate " // comment" <> None then failwith "expected empty input";
  check "Type" ~type_:"Type" ~value:"Type";
  check "((Type))" ~type_:"Type" ~value:"Type";
  check "x : Type" ~type_:"Type" ~value:"(self x. Type)";
  check "f(x : Type) : x" ~type_:"Type"
    ~value:"(self f. (Pi x : (self x. Type). x))";
  check "f(x : Type) = x" ~type_:"(self f. (Pi x : (self x. Type). Type))"
    ~value:"(fix f. (lambda x. x))";
  check "(f(x : Type) = x)(Type)" ~type_:"Type" ~value:"Type";
  check "(id(A : Type)(x : A) = x)(Type)(Type)" ~type_:"Type" ~value:"Type";
  check "(const(A : Type)(x : A)(y : A) = x)(Type)(Type)(Type)" ~type_:"Type"
    ~value:"Type";
  check "(id(A : Type)(x : A) = x)(f(x : Type) : Type)(g(x : Type) = x)(Type)"
    ~type_:"Type" ~value:"Type";
  check "f(x : Type)(x : Type) = x"
    ~type_:"(self f. (Pi x : (self x. Type). (Pi x1 : (self x1. Type). Type)))"
    ~value:"(fix f. (lambda x. (lambda x1. x1)))";
  check "f(x : Type) : Type = f(x)"
    ~type_:"(self f. (Pi x : (self x. Type). Type))"
    ~value:"(fix f. (lambda x. (f x)))";
  reject "Type(Type)" "unify: type clash";
  reject "Type; Type" "syntax error";
  List.iter
    (fun (source, message) -> reject source message)
    [
      ("()", "check_term: unit");
      ("[]", "check_term: boxes");
      ("Type, Type", "check_term: pairs");
      ("x : Type, Type", "check_term: sigma");
      ("f(()) = Type", "infer_pat: unit");
      ("f(x, y) = Type", "infer_pat: pairs");
      ("f([x]) = Type", "infer_pat: boxes");
      ("42", "parse_term: number");
      ("\"hello\"", "parse_term: string");
      ("Type | Type", "parse_term: union");
      ("Type & Type", "parse_term: intersection");
      ("{}", "parse_term: braces");
      ("_", "parse_term: hole");
    ];
  test_locations ();
  (* A failed line must not affect a subsequent elaboration. *)
  reject "(f(x) = x)(Type)" "unify: not a pattern";
  check "(f(x : Type) = x)(Type)" ~type_:"Type" ~value:"Type"
