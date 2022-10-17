type test = { name : string; expr : string; type_ : string; wrapper : bool }

let type_expr ?(wrapper = true) name ~type_ ~expr =
  { name; expr; type_; wrapper }

let id =
  type_expr "id" ~wrapper:false ~type_:"(A: *) -> (x: A) -> A"
    ~expr:"(A: *) => (x: A) => x"

let sequence =
  type_expr "sequence" ~wrapper:false
    ~type_:"(A: *) -> (x: A) -> (B: *) -> (y: B) -> B"
    ~expr:"(A: *) => (x: A) => (B: *) => (y: B) => y"

let bool =
  type_expr "bool" ~wrapper:false ~type_:"(A: *) -> (x: A) -> (y: A) -> A"
    ~expr:"(A: *) => (x: A) => (y: A) => x"

let true_ =
  type_expr "true" ~wrapper:false ~type_:"(A: *) -> (x: A) -> (y: A) -> A"
    ~expr:"(A: *) => (x: A) => (y: A) => x"

let false_ =
  type_expr "false" ~wrapper:false ~type_:"(A: *) -> (x: A) -> (y: A) -> A"
    ~expr:"(A: *) => (x: A) => (y: A) => y"

let incr = type_expr "incr" ~type_:"(x: Int) -> Int" ~expr:"(x: Int) => x"
let id_int = type_expr "id_int" ~type_:"(x: Int) -> Int" ~expr:"id Int"
let id_int_one = type_expr "id_int_one" ~type_:"Int" ~expr:"id Int one"

let bool_id =
  type_expr "bool_id" ~type_:"(x: Id) -> (y: Id) -> Id" ~expr:"bool Id"

let bool_id_id =
  type_expr "bool_id_id" ~type_:"(x: Id) -> Id" ~expr:"bool Id id"

let bool_id_id_id = type_expr "bool_id_id_id" ~type_:"Id" ~expr:"bool Id id id"

let true_type =
  type_expr "true_type" ~type_:"(x: *) -> (y: *) -> *" ~expr:"true *"

let true_type_int =
  type_expr "true_type_int" ~type_:"(y: *) -> *" ~expr:"true * Int"

let true_type_int_id =
  type_expr "true_type_int_id" ~type_:"*" ~expr:"true * Int Id"

let pred_dependent_type =
  type_expr "pred_dependent_type"
    ~type_:"(pred: Bool) -> (x: pred * Int Id) -> pred * Int Id"
    ~expr:"(pred: Bool) => (x: pred * Int Id) => x"

let true_dependent_type =
  type_expr "true_dependent_type" ~type_:"(x: Int) -> Int"
    ~expr:"((pred: Bool) => (x: pred * Int Id) => x) true"

let pair =
  type_expr "pair" ~wrapper:false
    ~type_:"(A: *) -> (B: *) -> (x: A) -> (y: B) -> (x: A, B)"
    ~expr:"(A: *) => (B: *) => (x: A) => (y: B) => (x = x, y : B)"

let left_unpair =
  type_expr "left_unpair" ~wrapper:false
    ~type_:"(A: *) -> (B: *) -> (x: A) -> (y: B) -> A"
    ~expr:
      {|(A: *) => (B: *) => (x: A) => (y: B) => (
          p = (x = x, y : B);
          (y, x) = p;
          y
        )|}

let right_unpair =
  type_expr "right_unpair" ~wrapper:false
    ~type_:"(A: *) -> (B: *) -> (x: A) -> (y: B) -> B"
    ~expr:
      {|(A: *) => (B: *) => (x: A) => (y: B) => (
          p = (x = x, y : B);
          (y, x) = p;
          x
        )|}

(* TODO: something like pack *)
(* let pack =
   type_expr "pack" ~wrapper:false
     ~type_:"(R: *) -> (A: *, r: R) -> (A: *, r: R)"
     ~expr:"(R: *) => (p: (A: *, x: R)) => p" *)

let pair_int =
  type_expr "pair_int" ~type_:"(A: *) -> (fst: Int) -> (snd: A) -> (x: Int, A)"
    ~expr:"pair Int"

let pair_int_int =
  type_expr "pair_int_int" ~type_:"(fst: Int) -> (snd: Int) -> (x: Int, Int)"
    ~expr:"pair Int Int"

let pair_int_int_one =
  type_expr "pair_int_int_one" ~type_:"(snd: Int) -> (x: Int, Int)"
    ~expr:"pair Int Int one"

let pair_int_int_one_one =
  type_expr "pair_int_int_one_one" ~type_:"(x: Int, Int)"
    ~expr:"pair Int Int one one"

let exists_a_a =
  type_expr "exists_a_a" ~type_:"(A : *, A)" ~expr:"(A = Int, one : A)"

let utils = [ id; sequence; bool; true_; false_; pair (* ;pack *); incr ]

let tests =
  [
    id;
    sequence;
    bool;
    true_;
    false_;
    incr;
    id_int;
    id_int_one;
    bool_id;
    bool_id_id;
    bool_id_id_id;
    true_type;
    true_type_int;
    true_type_int_id;
    pred_dependent_type;
    true_dependent_type;
    (* pack; *)
    pair;
    pair_int;
    pair_int_int;
    pair_int_int_one;
    pair_int_int_one_one;
    (* unpair *)
    left_unpair;
    right_unpair;
    exists_a_a;
  ]

open Smol

let parse_term term =
  let term_opt = Slexer.from_string Sparser.term_opt term in
  Option.get term_opt

let type_term env term =
  let term = parse_term term in
  let term = Lparser.from_stree term in
  Typer.type_term env term

let equal_type =
  Alcotest.testable
    (fun fmt typ -> Term.pp fmt typ)
    (fun a b ->
      let open Machinery in
      try
        equal ~expected:a ~received:b;
        equal ~expected:b ~received:a;
        true
      with _ -> false)

let wrapped_env =
  lazy
    (let open Term in
    let open Env in
    let var s = Var.create (Name.make s) in
    let env = empty in

    let env =
      let int_var = var "Int" in
      let int = t_var ~var:int_var ~type_:t_type in

      let env = enter int_var int env in
      let one_var = var "one" in
      enter one_var (t_var ~var:one_var ~type_:int) env
    in

    List.fold_left
      (fun env { name; type_; expr; wrapper = _ } ->
        let type_ = type_term env type_ in
        let expr = type_term env expr in

        let upper_name = String.capitalize_ascii name in
        let env = Env.enter (var upper_name) type_ env in
        Env.enter (var name) expr env)
      env utils)

let test { name; type_; expr; wrapper } =
  let check () =
    let env = Env.empty in
    let env = if wrapper then Lazy.force wrapped_env else env in

    let type_ = type_term env type_ in
    let expr = type_term env expr in
    let expr = Machinery.typeof expr in

    Alcotest.check equal_type "type" type_ expr
  in
  Alcotest.test_case name `Quick check

let test_utils = ("utils", List.map test utils)
let tests = ("tests", List.map test tests)
let () = Alcotest.run "Typer" [ test_utils; tests ]
