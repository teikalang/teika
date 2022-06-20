open Syntax

type test =
  | Fails of { (* TODO: check also the error *)
               name : string; code : string }
  | Equal of { name : string; code : string; type_ : string }
  | Subtype of { name : string; code : string; type_ : string }

let equal ~name ~code ~type_ = Equal { name; code; type_ }
let subtype ~name ~code ~type_ = Subtype { name; code; type_ }
let fails ~name ~code = Fails { name; code }

(* TODO: test ppx *)
let id = equal ~name:"id" ~code:"x => x" ~type_:"{A} -> A -> A"

let explicit_id =
  equal ~name:"explicit_id" ~code:"{A} => (x: A) => x" ~type_:"{A} -> A -> A"

let sequence =
  equal ~name:"sequence" ~code:"a => b => b" ~type_:"{A} -> A -> {B} -> B -> B"

let sequence_to_left =
  equal ~name:"sequence_to_left"
    ~code:{|sequence a b = b;
          (sequence: {A} -> {B} -> A -> B -> B)|}
    ~type_:"{A} -> {B} -> A ->  B -> B"

let sequence_to_right =
  fails ~name:"sequence_to_right"
    ~code:
      {|sequence a b = b;
        sequence_weak: {A} -> {B} -> A -> B -> B = sequence;
        (sequence_weak: {A} -> A -> {B} -> B -> B)|}

let choose =
  equal ~name:"choose" ~code:"((a => b => b) : {A} -> A -> A -> A)"
    ~type_:"{A} -> A -> A -> A"

let choose_id =
  equal ~name:"choose_id"
    ~code:
      {|choose: {A} -> A -> A -> A = a => b => b;
        id x = x;
        choose id|}
    ~type_:"({A} -> A -> A) -> {A} -> A -> A"

let choose_id_id =
  equal ~name:"choose_id_id"
    ~code:
      {|choose: {A} -> A -> A -> A = a => b => b;
        id x = x;
        choose_id = choose id;
        choose_id (x => x)|}
    ~type_:"{A} -> A -> A"

let choose_id_incr =
  fails ~name:"choose_id_incr"
    ~code:
      {|choose: {A} -> A -> A -> A = a => b => b;
        incr (x: Int) = x;
        id x = x;
        choose_id = choose id;
        choose_id incr|}

let choose_id_hm =
  equal ~name:"choose_id_hm"
    ~code:
      {|choose: {A} -> A -> A -> A = a => b => b;
        choose_id_hm: {A} -> (A -> A) -> (A -> A) -> A -> A = choose;
        id x = x;
        choose_id_hm id|}
    ~type_:"{A} -> (A -> A) -> A -> A"

let choose_id_hm_incr =
  equal ~name:"choose_id_hm_incr"
    ~code:
      {|choose: {A} -> A -> A -> A = a => b => b;
        choose_id_hm: {A} -> (A -> A) -> (A -> A) -> A -> A = choose;
        incr (x: Int) = x;
        id x = x;
        choose_id_hm id incr |}
    ~type_:"Int -> Int"

let number_types =
  equal ~name:"number_types" ~code:"x => 1" ~type_:"{A} -> A -> Int"

let empty_record_type =
  equal ~name:"empty_record_type" ~code:"(x: {}) => x" ~type_:"({}) -> {}"

let multiple_fields_record =
  equal ~name:"multiple_fields_record" ~code:"(m: { x: Int; y: Int; }) => m"
    ~type_:"({ x: Int; y: Int; }) -> { x: Int; y: Int; }"

let module_is_not_value =
  fails ~name:"module_is_not_value" ~code:"id (x: Int) = x; id Int"

let term_type_alias =
  equal ~name:"term_type_alias" ~code:"T = Int; (1: Int)" ~type_:"Int"

let type_type_alias =
  equal ~name:"type_type_alias" ~code:"1" ~type_:"(T = Int; T)"

let kind_alias =
  equal ~name:"kind_alias" ~code:"K = *; (A: K) => (x: A) => x"
    ~type_:"(A: *) -> A -> A"

let deep_kind_alias =
  equal ~name:"deep_kind_alias"
    ~code:{|K = K = *; K;
            (A: K) => (x: A) => x|}
    ~type_:"(A: *) -> A -> A"

let term_type_function =
  equal ~name:"term_type_function" ~code:"Id = X => X; (1: (Id Int))"
    ~type_:"Int"

let type_type_function =
  equal ~name:"type_type_function" ~code:"1" ~type_:"(Id = X => X; Id Int)"

let term_wrong_type_function =
  fails ~name:"term_wrong_type_function" ~code:"Id = X => X; ({}: Id Int)"

let type_wrong_type_function =
  fails ~name:"type_wrong_type_function" ~code:"({}: (Id = X => X; Id Int))"

let polymorphism_rank2 =
  equal ~name:"polymorphism_rank2"
    ~code:"(Id: ({A} -> A -> A)) => (Id 1: Id Int)"
    ~type_:"({A} -> A -> A) -> Int"

(* TODO: I think this lambda should not be supported in this place, why? *)
let cursed_polymorphism_rank2 =
  equal ~name:"cursed_polymorphism_rank2"
    ~code:"(Id: X => X) => (Id 1: (Id Int))" ~type_:"(X => X) -> Int"

let dont_lower_var =
  equal ~name:"dont_lower_var" ~code:"{A} => {B} => (T: A -> B) => T"
    ~type_:"{A} -> {B} -> (A -> B) -> A -> B"

let cursed_destruct_arrow_param =
  equal ~name:"cursed_destruct_arrow_param" ~code:"1"
    ~type_:"(F = {A} => {B} => (T: A -> B) => A; f x = 1; F f)"

let cursed_destruct_arrow_return =
  equal ~name:"cursed_destruct_arrow_return" ~code:"1"
    ~type_:"(F = {A} => {B} => (T: A -> B) => B; f x = 1; F f)"

let simple_record =
  equal ~name:"simple_record" ~code:"{ x = 1; }" ~type_:"{ x: Int; }"

let explicit_type =
  equal ~name:"explicit_type" ~code:"(A: *) => (x: A) => x"
    ~type_:"(A: *) -> A -> A"

let calling_explicit_type =
  equal ~name:"calling_explicit_type"
    ~code:{|explicit_id = (A: *) => (x: A) => x;
            explicit_id Int|}
    ~type_:"Int -> Int"

let escape_scope =
  fails ~name:"escape_scope"
    ~code:
      {|explicit_id (A: *) (x: A) = x;
        x => (A: *) => explicit_id A x|}

let explicit_type_constructor =
  equal ~name:"explicit_type_constructor" ~code:"(A: *) => A"
    ~type_:"(A: *) -> A"

(* TODO: solve semicolons problem *)
let type_record_id =
  equal ~name:"type_record_id" ~code:"({ A; }: { A: *; }) => (x: A) => x"
    ~type_:"({ A; }: { A: *; }) -> A -> A"

let calling_type_record_id =
  equal ~name:"calling_type_record_id"
    ~code:
      {|explicit_record_id = ({ A; }: { A: *; }) => (x: A) => x;
        explicit_record_id { A = Int; }|}
    ~type_:"Int -> Int"

(* TODO: is this unsound even if called with a non struct type? *)

let existential_record =
  subtype ~name:"existential_record" ~code:"{ T = Int; x = 1; }"
    ~type_:"{ T: *; x: T; }"

let double_existential_record =
  subtype ~name:"let_value_existential_record"
    ~code:"({ T = Int; x = 1; }: { T: *; x: T; })" ~type_:"{ T: *; x: T; }"

let let_type_existential_record =
  subtype ~name:"let_type_existential_record" ~code:"{ T = Int; x = 1; }"
    ~type_:"S = { T: *; x: T; }; S"

let let_value_existential_record =
  subtype ~name:"let_value_existential_record"
    ~code:{|S = { T: *; x: T; };
            ({ T = Int; x = 1; }: S)|}
    ~type_:"{ T: *; x: T; }"

let type_abstraction =
  equal ~name:"type_abstraction"
    ~code:
      {|S = { T: *; x: T; to_int: T -> Int; };
        { T; x; to_int; }: S = { T = Int; x = 1; to_int x = x; };
        to_int (x: T)|}
    ~type_:"Int"

let type_abstraction_fail =
  fails ~name:"type_abstraction_fail"
    ~code:{|{ T; x; }: { T: *; x: T; } = { T = Int; x = 1; }; (x: Int)|}

let infer_kind_id =
  equal ~name:"infer_kind_id" ~code:"A => (x: A) => x" ~type_:"(A: *) -> A -> A"

let deep_type_abstraction =
  subtype ~name:"deep_type_abstraction"
    ~code:"{ M: { T: *; x: T; } = { T = Int; x = 1; }; }"
    ~type_:"{ M: { T: *; x: T; }; }"

let deep_type_abstraction_fail =
  fails ~name:"deep_type_abstraction_fail"
    ~code:
      {|{ M; } = { M: { T: *; x: T; } = { T = Int; x = 1; }; };
        { T; x; } = M;
        (x: Int)|}

open Typer

let annot ~expected ~received =
  Syntax.
    {
      s_loc = Location.none;
      s_desc = S_annot { value = received; type_ = expected };
    }

let type_expr env syntax =
  let lang = Language.interpret_expr syntax in
  type_expr env lang

let type_type env syntax =
  let lang = Language.interpret_expr syntax in
  type_type env lang

let equal_type env =
  Alcotest.testable
    (fun fmt (typ, _) -> Print.pp_type_debug fmt typ)
    (fun (a, _) (b, _) ->
      let open Unify in
      (* TODO: only works because there is no weak var  *)
      try
        unify env ~expected:a ~received:b;
        unify env ~expected:b ~received:a;
        true
      with Unify.Error _ -> false)

let subtype_type env =
  Alcotest.testable
    (fun fmt (typ, _) -> Print.pp_type_debug fmt typ)
    (fun (_, expected) (_, received) ->
      (* TODO: only works because there is no weak var  *)
      let _ = type_expr env (annot ~expected ~received) in
      true)

let value_from_string ~name string =
  match value_from_string string with
  | Some received -> received
  | None -> failwith ("string likely empty: " ^ name)

let test_match_type ~equal ~name ~code ~type_ =
  let check () =
    let env = Env.base in
    let type_ = value_from_string ~name type_ in
    let type_type, _type = type_type env type_ in
    let code = value_from_string ~name code in
    let code_type =
      let forall, env = Env.enter_forall env in
      let code_type, _code = type_expr env code in
      Typer.Forall.universal forall;
      Typer.Type.new_forall forall ~body:code_type
    in

    let checker = if equal then equal_type else subtype_type in
    Alcotest.check (checker env) name (type_type, type_) (code_type, code)
  in
  Alcotest.test_case name `Quick check

let test_unify_fails ~name ~code =
  let check () =
    let env = Env.base in
    let code = value_from_string ~name code in
    let actual =
      try
        let _ = type_expr env code in
        false
      with
      | Unify.Error _ -> true
      | Typer.Type_term.Error { error = Invalid_number | Not_a_type; loc = _ }
        ->
          true
    in

    Alcotest.(check bool name true actual)
  in
  Alcotest.test_case name `Quick check

let test case =
  match case with
  | Fails { name; code } -> test_unify_fails ~name ~code
  | Equal { name; code; type_ } ->
      test_match_type ~equal:true ~name ~code ~type_
  | Subtype { name; code; type_ } ->
      test_match_type ~equal:false ~name ~code ~type_

let _tests =
  [
    cursed_polymorphism_rank2;
    explicit_type_constructor;
    (* regressions *)
    kind_alias;
    deep_kind_alias;
  ]

(* TODO: generate test, IDENTICAL (x: t = e1; e2) == ((x: t) => e1) e2 *)

let tests =
  [
    id;
    explicit_id;
    sequence;
    sequence_to_left;
    sequence_to_right;
    choose;
    choose_id;
    choose_id_id;
    choose_id_incr;
    choose_id_hm;
    choose_id_hm_incr;
    number_types;
    empty_record_type;
    multiple_fields_record;
    module_is_not_value;
    term_type_alias;
    type_type_alias;
    term_type_function;
    type_type_function;
    term_wrong_type_function;
    type_wrong_type_function;
    polymorphism_rank2;
    dont_lower_var;
    cursed_destruct_arrow_param;
    cursed_destruct_arrow_return;
    simple_record;
    explicit_type;
    calling_explicit_type;
    escape_scope;
    type_record_id;
    calling_type_record_id;
    existential_record;
    double_existential_record;
    let_type_existential_record;
    let_value_existential_record;
    type_abstraction;
    type_abstraction_fail;
    infer_kind_id;
    deep_type_abstraction;
    deep_type_abstraction_fail;
  ]

let tests = ("tests", List.map test tests)
let () = Alcotest.run "Typer" [ tests ]

include Typer
