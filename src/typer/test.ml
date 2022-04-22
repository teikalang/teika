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
    | S_lambda of { param : term; body : term }
    | S_apply of { lambda : term; arg : term }
    | S_bind of { bound : term; value : term option; body : term option }
    | S_struct of term option
    | S_field of { struct_ : term; field : term }
    | S_match of { value : term; pat : term; body : term }
    | S_annot of { value : term; type_ : term }
  [@@deriving show { with_path = false }]
end

let make desc = { s_loc = Location.none; s_desc = desc }
let var name = make (S_ident name)
let number number = make (S_number number)
let ( @-> ) param body = make (S_lambda { param; body })
let app lambda arg = make (S_apply { lambda; arg })
let bind bound value body = make (S_bind { bound; value; body })
let struct_ content = make (S_struct content)
let field struct_ field = make (S_field { struct_; field })
let match_ value pat body = make (S_match { value; pat; body })
let annot value type_ = make (S_annot { value; type_ })
let parse code = Syntax.value_from_string code |> Option.get
let env = Env.empty

type test = {
  (* TODO: check also the error *)
  name : string;
  code : string;
  type_ : string option;
}

let works ~name ~code ~type_ = { name; code; type_ = Some type_ }
let fails ~name ~code = { name; code; type_ = None }

(* TODO: test ppx *)
let id = works ~name:"id" ~code:"x -> x" ~type_:"{A} -> A -> A"

let sequence =
  works ~name:"sequence" ~code:"a -> b -> b" ~type_:"{A} -> A -> {B} -> B -> B"

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
  works ~name:"choose" ~code:"((a -> b -> b) : {A} -> A -> A -> A)"
    ~type_:"{A} -> A -> A -> A"

let choose_id =
  works ~name:"choose_id"
    ~code:
      {|choose: {A} -> A -> A -> A = (a -> b -> b);
          id x = x;
          choose id|}
    ~type_:"({A} -> A -> A) -> {A} -> A -> A"

let choose_id_id =
  works ~name:"choose_id_id"
    ~code:
      {|choose: {A} -> A -> A -> A = (a -> b -> b);
          id x = x;
          choose_id = choose id;
          choose_id (x -> x)|}
    ~type_:"{A} -> A -> A"

let choose_id_incr =
  fails ~name:"choose_id_incr"
    ~code:
      {|choose: {A} -> A -> A -> A = (a -> b -> b);
              incr (x: Int) = x;
              id x = x;
              choose_id = choose id;
              choose_id incr|}

let choose_id_hm =
  works ~name:"choose_id_hm"
    ~code:
      {|choose: {A} -> A -> A -> A = (a -> b -> b);
              choose_id_hm: {A} -> (A -> A) -> (A -> A) -> A -> A = choose;
              id x = x;
              choose_id_hm id|}
    ~type_:"{A} -> (A -> A) -> A -> A"

let choose_id_hm_incr =
  works ~name:"choose_id_incr_hm"
    ~code:
      {|choose: {A} -> A -> A -> A = (a -> b -> b);
            choose_id_hm: {A} -> (A -> A) -> (A -> A) -> A -> A = choose;
            incr (x: Int) = x;
            id x = x;
            choose_id_hm id incr |}
    ~type_:"Int -> Int"

let number_types =
  works ~name:"number_types" ~code:"x -> 1" ~type_:"{A} -> A -> Int"

open Typer

let equal_type env =
  Alcotest.testable pp_type (fun a b ->
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
    let code = value_from_string ~name code in
    let code, _code = type_expr env code in

    let type_ = value_from_string ~name type_ in
    let type_, _type = transl_type env type_ in

    Alcotest.check (equal_type env) name type_ code
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
      with Unify.Error _ -> true
    in

    Alcotest.(check bool name true actual)
  in
  Alcotest.test_case name `Quick check

let test { name; code; type_ } =
  match type_ with
  | Some type_ -> test_equal_type ~name ~code ~type_
  | None -> test_unify_fails ~name ~code

let tests =
  [
    id;
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
  ]

let tests = ("tests", List.map test tests)
let () = Alcotest.run "Typer" [ tests ]

include Typer
