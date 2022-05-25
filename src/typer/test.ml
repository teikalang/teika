open Syntax

type test = {
  (* TODO: check also the error *)
  name : string;
  code : string;
  type_ : string option;
}

let works ~name ~code ~type_ = { name; code; type_ = Some type_ }
let fails ~name ~code = { name; code; type_ = None }

(* TODO: test ppx *)
let id = works ~name:"id" ~code:"x => x" ~type_:"{A} -> A -> A"

let explicit_id =
  works ~name:"explicit_id" ~code:"{A} => (x: A) => x" ~type_:"{A} -> A -> A"

let sequence =
  works ~name:"sequence" ~code:"a => b => b" ~type_:"{A} -> A -> {B} -> B -> B"

let sequence_to_left =
  works ~name:"sequence_to_left"
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
  works ~name:"choose" ~code:"((a => b => b) : {A} -> A -> A -> A)"
    ~type_:"{A} -> A -> A -> A"

let choose_id =
  works ~name:"choose_id"
    ~code:
      {|choose: {A} -> A -> A -> A = a => b => b;
        id x = x;
        choose id|}
    ~type_:"({A} -> A -> A) -> {A} -> A -> A"

let choose_id_id =
  works ~name:"choose_id_id"
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
  works ~name:"choose_id_hm"
    ~code:
      {|choose: {A} -> A -> A -> A = a => b => b;
        choose_id_hm: {A} -> (A -> A) -> (A -> A) -> A -> A = choose;
        id x = x;
        choose_id_hm id|}
    ~type_:"{A} -> (A -> A) -> A -> A"

let choose_id_hm_incr =
  works ~name:"choose_id_hm_incr"
    ~code:
      {|choose: {A} -> A -> A -> A = a => b => b;
        choose_id_hm: {A} -> (A -> A) -> (A -> A) -> A -> A = choose;
        incr (x: Int) = x;
        id x = x;
        choose_id_hm id incr |}
    ~type_:"Int -> Int"

let number_types =
  works ~name:"number_types" ~code:"x => 1" ~type_:"{A} -> A -> Int"

let empty_struct_type =
  works ~name:"empty_struct_type" ~code:"(x: {}) => x" ~type_:"({}) -> {}"

let multiple_fields_struct =
  works ~name:"multiple_fields_struct" ~code:"(m: { x: Int; y: Int; }) => m"
    ~type_:"({ x: Int; y: Int; }) -> { x: Int; y: Int; }"

let module_is_not_value =
  fails ~name:"module_is_not_value" ~code:"id (x: Int) = x; id Int"

let term_type_alias =
  works ~name:"term_type_alias" ~code:"T = Int; (1: Int)" ~type_:"Int"

let type_type_alias =
  works ~name:"type_type_alias" ~code:"1" ~type_:"(T = Int; T)"

let term_type_function =
  works ~name:"term_type_function" ~code:"Id = X => X; (1: (Id Int))"
    ~type_:"Int"

let type_type_function =
  works ~name:"type_type_function" ~code:"1" ~type_:"(Id = X => X; Id Int)"

let term_wrong_type_function =
  fails ~name:"term_wrong_type_function" ~code:"Id = X => X; ({}: Id Int)"

let type_wrong_type_function =
  fails ~name:"type_wrong_type_function" ~code:"({}: (Id = X => X; Id Int))"

let polymorphism_rank2 =
  works ~name:"polymorphism_rank2"
    ~code:"(Id: ({A} -> A -> A)) => (Id 1: Id Int)"
    ~type_:"({A} -> A -> A) -> Int"

(* TODO: I think this lambda should not be supported in this place, why? *)
let cursed_polymorphism_rank2 =
  works ~name:"cursed_polymorphism_rank2"
    ~code:"(Id: X => X) => (Id 1: (Id Int))" ~type_:"(X => X) -> Int"

let dont_lower_var =
  works ~name:"dont_lower_var" ~code:"{A} => {B} => (T: A -> B) => T"
    ~type_:"{A} -> {B} -> (A -> B) -> A -> B"

let cursed_destruct_arrow_param =
  works ~name:"cursed_destruct_arrow_param" ~code:"1"
    ~type_:"(F = {A} => {B} => (T: A -> B) => A; f x = 1; F f)"

let cursed_destruct_arrow_return =
  works ~name:"cursed_destruct_arrow_return" ~code:"1"
    ~type_:"(F = {A} => {B} => (T: A -> B) => B; f x = 1; F f)"

let simple_struct =
  works ~name:"simple_struct" ~code:"{ x = 1; }" ~type_:"{ x: Int; }"

let explicit_type =
  works ~name:"explicit_type" ~code:"(A: *) => (x: A) => x"
    ~type_:"(A: *) -> A -> A"

let calling_explicit_type =
  works ~name:"calling_explicit_type"
    ~code:{|explicit_id = (A: *) => (x: A) => x;
            explicit_id Int|}
    ~type_:"Int -> Int"

let explicit_type_constructor =
  works ~name:"explicit_type_constructor" ~code:"(A: *) => A"
    ~type_:"(A: *) -> A"

open Typer

let equal_type env =
  Alcotest.testable Print.pp_type_debug (fun a b ->
      let open Unify in
      let loc = Location.none in
      (* TODO: only works because there is no weak var  *)
      try
        unify ~loc env ~expected:a ~received:b;
        unify ~loc env ~expected:b ~received:a;
        true
      with Unify.Error _ -> false)

let value_from_string ~name string =
  match value_from_string string with
  | Some received -> received
  | None -> failwith ("string likely empty: " ^ name)

let test_equal_type ~name ~code ~type_ =
  let check () =
    let env = Env.base in
    let type_ = value_from_string ~name type_ in
    let type_, _type, _env = type_type env type_ in
    let code = value_from_string ~name code in
    let code, _code, _env =
      let forall = Type.Forall_id.next () in
      let env = Env.enter_forall ~forall env in
      type_term env code
    in
    (* TODO: this generalize makes sense? *)
    let code = Typer.Generalize.generalize env code in

    Alcotest.check (equal_type env) name type_ code
  in
  Alcotest.test_case name `Quick check

let test_unify_fails ~name ~code =
  let check () =
    let env = Env.base in
    let code = value_from_string ~name code in
    let actual =
      try
        let _ = type_term env code in
        false
      with
      | Unify.Error _ -> true
      | Typer.Type_term.Error _ -> true
    in

    Alcotest.(check bool name true actual)
  in
  Alcotest.test_case name `Quick check

let test { name; code; type_ } =
  match type_ with
  | Some type_ -> test_equal_type ~name ~code ~type_
  | None -> test_unify_fails ~name ~code

let _tests = [ cursed_polymorphism_rank2; explicit_type_constructor ]

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
    empty_struct_type;
    multiple_fields_struct;
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
    simple_struct;
    explicit_type;
    calling_explicit_type;
  ]

let tests = ("tests", List.map test tests)
let () = Alcotest.run "Typer" [ tests ]

include Typer
