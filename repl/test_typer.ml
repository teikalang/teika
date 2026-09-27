open Repl_core
module C = Typer.Typed_tree
module L = Typer.Level

let loc =
  match Lexer.parse "Type" with
  | Some (Lparser.Syntax_tree.Term { loc; _ }) -> loc
  | None -> failwith "missing test source location"

let type_ () = C.t_type loc

let var ?(at = L.succ L.zero) name =
  C.v_fresh loc ~name:(Ctree.Name.make name) ~at ()

let flex ?(at = L.zero) () = C.t_flex loc ~at ()
let lambda = C.t_lambda loc
let apply = C.t_apply loc
let variable = C.t_var loc

let expect_type term =
  match C.t_head (C.whnf term) with
  | C.T_type -> ()
  | _ -> failwith "expected Type"

let expect_var expected term =
  match C.t_head (C.whnf term) with
  | C.T_var { var } when C.v_same expected var -> ()
  | _ -> failwith "expected the same rigid variable"

let expect_error prefix action =
  match action () with
  | () -> failwith ("expected " ^ prefix)
  | exception Typer.Error (location, message) ->
      if location.loc_ghost || not (String.starts_with ~prefix message) then
        failwith ("unexpected error: " ^ message)

let test_substitution () =
  let a = var "A" in
  let x = var "x" in
  let identity = lambda x (variable x) in
  (* Substituting for a lambda's own binder must preserve the lambda. *)
  (match C.t_head (C.subst x (type_ ()) identity) with
  | C.T_lambda _ -> ()
  | _ -> failwith "substitution dropped a shadowing lambda");
  let dependent = C.t_pi loc (variable a) (lambda x (variable a)) in
  let specialized = C.subst a (type_ ()) dependent in
  (match C.t_head specialized with
  | C.T_pi { param; body } ->
      expect_type param;
      expect_type (apply body (type_ ()))
  | _ -> failwith "expected a specialized dependent function type");
  (* The free replacement variable must not be captured by the inner binder. *)
  let outer = var "outer" in
  let inner = var "inner" in
  let original = lambda inner (variable outer) in
  let replaced = C.subst outer (variable inner) original in
  (match C.t_head replaced with
  | C.T_lambda { bound; body } ->
      if C.v_same bound inner then
        failwith "substitution did not freshen binder";
      expect_var inner body
  | _ -> failwith "expected lambda after capture-avoiding substitution");
  let shared = variable outer in
  let let_ = C.t_let loc inner (type_ ()) shared in
  expect_type (C.subst outer (type_ ()) let_);
  expect_var outer shared;
  (* A local alias can capture a surrounding function parameter. Substitution
     must reach that parameter without changing the alias's definition. *)
  let alias =
    C.v_fresh loc ~to_:shared ~name:(Ctree.Name.make "alias")
      ~at:(L.succ (C.v_at outer))
      ()
  in
  expect_type (C.subst outer (type_ ()) (variable alias));
  expect_var outer (variable alias);
  (* Freshening nested binders must not repeatedly traverse the same body. *)
  let binders = List.init 24 (fun index -> var ("x" ^ string_of_int index)) in
  let nested = List.fold_right lambda binders (variable outer) in
  let rec check_nested = function
    | 0 -> expect_type
    | count -> (
        fun term ->
          match C.t_head term with
          | C.T_lambda { body; _ } -> check_nested (count - 1) body
          | _ -> failwith "substitution lost a nested binder")
  in
  check_nested (List.length binders) (C.subst outer (type_ ()) nested)

let test_unification () =
  let expected = flex () in
  C.unify (type_ ()) expected;
  expect_type expected;
  let received = flex () in
  C.unify received (type_ ());
  expect_type received;
  let early = flex () in
  let late = flex ~at:(L.succ L.zero) () in
  C.unify early late;
  if not L.(C.t_at late <= L.zero) then failwith "flex levels were not lowered";
  let scoped = flex () in
  let escaped = var "escaped" in
  expect_error "unify: escape check" (fun () ->
      C.unify scoped (variable escaped));
  let recursive = flex () in
  expect_error "unify: occurs check" (fun () ->
      C.unify recursive (C.t_pi loc (type_ ()) recursive));
  let x = var "x" in
  let y = var "y" in
  C.unify (lambda x (variable x)) (lambda y (variable y));
  (* Solve F x y = x in argument order, with lambda-bound variables in scope. *)
  let function_type = C.t_pi loc (type_ ()) (lambda x (type_ ())) in
  let f = flex () in
  C.unify (apply (apply f (variable x)) (variable y)) (variable x);
  expect_type (apply (apply f (type_ ())) function_type);
  let duplicate = flex () in
  expect_error "unify: duplicated variable" (fun () ->
      C.unify (apply (apply duplicate (variable x)) (variable x)) (type_ ()));
  let non_pattern = flex () in
  expect_error "unify: not a pattern" (fun () ->
      C.unify (apply non_pattern (type_ ())) (type_ ()));
  let rigid = var "f" in
  (* The expected application spine must advance to its function. *)
  C.unify
    (apply (variable rigid) (type_ ()))
    (apply (variable rigid) (type_ ()));
  C.unify (lambda x (apply (variable rigid) (variable x))) (variable rigid);
  C.unify (variable rigid) (lambda y (apply (variable rigid) (variable y)));
  let solved = flex () in
  C.unify solved (lambda x (variable x));
  C.unify (apply solved (type_ ())) (type_ ());
  (* Reusing a term in independent branches is not a reduction cycle. *)
  let shared = C.t_lambda loc x (type_ ()) in
  C.unify (C.t_pi loc (type_ ()) shared) (C.t_pi loc (type_ ()) shared)

let test_fix () =
  let self = var "self" in
  let value = C.t_fix loc (lambda self (type_ ())) in
  C.unify value (type_ ());
  C.unify (type_ ()) value;
  let loop = C.t_fix loc (lambda self (variable self)) in
  expect_error "reduction limit exceeded" (fun () ->
      ignore (C.unify_at 100 loop (type_ ())))

let test_scoped_unfolding () =
  let recursive = var "R" in
  let definitions = [ (recursive, type_ ()) ] in
  C.unify ~definitions (variable recursive) (type_ ());
  C.unify ~definitions (type_ ()) (variable recursive);
  let x = var "x" in
  C.unify ~definitions
    (C.t_pi loc (variable recursive) (lambda x (variable recursive)))
    (C.t_pi loc (type_ ()) (lambda x (type_ ())));
  (* Conversion assumptions are local and do not link the rigid variable. *)
  expect_error "unify: type clash" (fun () ->
      C.unify (variable recursive) (type_ ()));
  let definitions = [ (recursive, variable recursive) ] in
  expect_error "reduction limit exceeded" (fun () ->
      ignore (C.unify_at ~definitions 100 (variable recursive) (type_ ())))

let test_initial_context () =
  let module E = Typer.Elaborate in
  let name = Ctree.Name.make "Type" in
  (match E.lookup (E.initial_context loc) name with
  | Some { var; received; _ } ->
      expect_type received;
      expect_type (variable var)
  | None -> failwith "Type is missing from the initial context");
  let syntax = Lparser.Syntax_tree.t_wrap loc (T_var { var = name }) in
  let empty = E.Context { vars = []; at = L.zero; definitions = [] } in
  expect_error "unbound variable: Type" (fun () ->
      E.check_term empty syntax (type_ ()) (flex ()));
  let shadow = var ~at:L.zero "Type" in
  let ctx =
    E.Context
      {
        vars = [ { name; var = shadow; received = type_ () } ];
        at = L.zero;
        definitions = [];
      }
  in
  let value = flex () in
  E.check_term ctx syntax (type_ ()) value;
  expect_var shadow value

let () =
  test_substitution ();
  test_unification ();
  test_fix ();
  test_scoped_unfolding ();
  test_initial_context ()
