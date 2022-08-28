type test = { name : string; expr : string; type_ : string; wrapper : bool }

let type_expr ?(wrapper = true) name ~type_ ~expr =
  { name; expr; type_; wrapper }

let id =
  type_expr "id" ~wrapper:false ~type_:"(A: *) -> A -> A"
    ~expr:"(A: *) => (x: A) => x"

let sequence =
  type_expr "sequence" ~wrapper:false ~type_:"(A: *) -> A -> (B: *) -> B -> B"
    ~expr:"(A: *) => (x: A) => (B: *) => (y: B) => y"

let choose =
  type_expr "choose" ~wrapper:false ~type_:"(A: *) -> A -> A -> A"
    ~expr:"(A: *) => (x: A) => (y: A) => x"

let incr = type_expr "incr" ~type_:"Int -> Int" ~expr:"(x: Int) => x"
let id_int = type_expr "id_int" ~type_:"Int -> Int" ~expr:"id Int"
let id_int_one = type_expr "id_int_one" ~type_:"Int" ~expr:"id Int one"
let choose_id = type_expr "choose_id" ~type_:"Id -> Id -> Id" ~expr:"choose Id"

let choose_id_id =
  type_expr "choose_id_id" ~type_:"Id -> Id" ~expr:"choose Id id"

let choose_id_id_id =
  type_expr "choose_id_id_id" ~type_:"Id" ~expr:"choose Id id id"

let pair =
  type_expr "pair" ~wrapper:false
    ~type_:"(A: *) -> (B: *) -> A -> B -> (x: A, y: B)"
    ~expr:"(A: *) => (B: *) => (x: A) => (y: B) => (x = x, y = y)"

let left_unpair =
  type_expr "left_unpair" ~wrapper:false
    ~type_:"(A: *) -> (B: *) -> A -> B -> A"
    ~expr:
      {|(A: *) => (B: *) => (x: A) => (y: B) => (
          p = (x = x, y = y);
          (y, x) = p;
          y
        )|}

let right_unpair =
  type_expr "right_unpair" ~wrapper:false
    ~type_:"(A: *) -> (B: *) -> A -> B -> B"
    ~expr:
      {|(A: *) => (B: *) => (x: A) => (y: B) => (
          p = (x = x, y = y);
          (y, x) = p;
          x
        )|}

(* TODO: something like pack *)
(* let pack =
   type_expr "pack" ~wrapper:false
     ~type_:"(R: *) -> (A: *, r: R) -> (A: *, r: R)"
     ~expr:"(R: *) => (p: (A: *, x: R)) => p" *)

let pair_int =
  type_expr "pair_int" ~type_:"(A: *) -> Int -> A -> (x: Int, y: A)"
    ~expr:"pair Int"

let pair_int_int =
  type_expr "pair_int_int" ~type_:"Int -> Int -> (x: Int, y: Int)"
    ~expr:"pair Int Int"

let pair_int_int_one =
  type_expr "pair_int_int_one" ~type_:"Int -> (x: Int, y: Int)"
    ~expr:"pair Int Int one"

let pair_int_int_one_one =
  type_expr "pair_int_int_one_one" ~type_:"(x: Int, y: Int)"
    ~expr:"pair Int Int one one"

(* let exists_a_a =
   type_expr "exists_a_a" ~type_:"(A: *, x: A)"
     ~expr:"pack #(A: *, x: A) (A = Int, x = one)" *)

let annot_pack =
  type_expr "annot_pack" ~type_:"(A: *, x: A)"
    ~expr:"((A = Int, x = one): (A: *, x: A))"

let alias_apply =
  type_expr "alias_function" ~type_:"Int -> Int"
    ~expr:"((A := Int) => (x: A) => incr x) Int"

let utils = [ id; sequence; choose; pair (* ;pack *); incr ]

let tests =
  [
    id;
    sequence;
    choose;
    incr;
    id_int;
    id_int_one;
    choose_id;
    choose_id_id;
    choose_id_id_id;
    pair;
    (* pack; *)
    pair_int;
    pair_int_int;
    pair_int_int_one;
    pair_int_int_one_one;
    (* unpair *)
    left_unpair;
    right_unpair;
    (* exists_a_a; *)
    annot_pack;
    (* alias *)
    alias_apply;
  ]

open Smol

let parse_expr expr = Lexer.from_string Parser.expr_opt expr |> Option.get
let parse_type expr = Lexer.from_string Parser.type_opt expr |> Option.get

let type_expr env expr =
  let expr = parse_expr expr in
  let expr = Type_expr.type_expr env expr in
  let (TE { type_ = expr; desc = _ }) = expr in
  expr

let type_type env type_ =
  let type_ = parse_type type_ in
  let type_ = Transl_type.transl_type env type_ in
  let (TT { type_; desc = _ }) = type_ in
  type_

let equal_type =
  Alcotest.testable
    (fun fmt typ -> Type.pp fmt typ)
    (fun a b ->
      let open Machinery in
      (* TODO: only works because there is no weak var  *)
      try
        subtype ~expected:a ~received:b;
        subtype ~expected:b ~received:a;
        true
      with _ -> false)

let wrapped_env =
  lazy
    (let open Type in
    let var s = Var.create (Name.make s) in
    let env = Env.empty in
    let int = var "Int" in
    let int_type = Type.t_var ~var:int in
    let env = Env.enter int (t_alias ~type_:int_type) env in
    let env = Env.enter (var "one") int_type env in

    List.fold_left
      (fun env { name; type_; expr; wrapper = _ } ->
        let type_ = type_type env type_ in
        let expr = type_expr env expr in

        let upper_name = String.capitalize_ascii name in
        let env = Env.enter (var upper_name) (t_alias ~type_) env in
        Env.enter (var name) expr env)
      env utils)

let test { name; type_; expr; wrapper } =
  let check () =
    let env = Env.empty in
    let env = if wrapper then Lazy.force wrapped_env else env in

    let type_ = type_type env type_ in
    let expr = type_expr env expr in

    Alcotest.check equal_type "type" type_ expr
  in
  Alcotest.test_case name `Quick check

let test_utils = ("utils", List.map test utils)
let tests = ("tests", List.map test tests)
let () = Alcotest.run "Typer" [ test_utils; tests ]
