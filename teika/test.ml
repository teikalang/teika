open Syntax

module Typer = struct
  open Teika

  type test =
    | Check of { name : string; annotated_term : string }
    | Fail of { name : string; annotated_term : string }

  let check name annotated_term = Check { name; annotated_term }
  let fail name annotated_term = Fail { name; annotated_term }

  (* TODO: write tests for locations and names / offset *)
  (* TODO: write tests for escape check *)
  let univ_type = check "Type" {|(Type : Type)|}
  let string_type = check "String" {|(String : Type)|}
  let false_type = check "False" {|(A : Type) -> A|}

  let id =
    check "id" {|((A : Type) => (x : A) => x : (A : Type) -> (x : A) -> A)|}

  let id_propagate =
    check "id_propagate" {|((A => x => x) : (A : Type) -> (x : A) -> A)|}

  let id_unify =
    check "id_unify" {|((A => (x : A) => x) : (A : Type) -> (x : A) -> A)|}

  let let_id =
    check "let_id"
      {|((
        id : (A : Type) -> (x : A) -> A = A => (x : A) => x;
        id
      ) : (A : Type) -> (x : A) -> A)|}

  let id_type =
    check "id_type"
      {|(((A : Type) => (x : A) => x) Type
        : (x : Type) -> Type)|}

  let id_type_never =
    check "id_type_never"
      {|(((A : Type) => (x : A) => x) Type ((A : Type) -> A)
        : Type)|}

  let return_id_propagate =
    check "return_id_propagate"
      {|((((id : (A : Type) -> (x : A) -> A) => id) (A => x => x))
        : (A : Type) -> (x : A) -> A)|}

  let sequence =
    check "sequence"
      {|((A => (x : A) => B => (y : B) => y)
        : (A : Type) -> (x : A) -> (B : Type) -> (y : B) -> B)|}

  let bool =
    check "bool" {|(((A : Type) -> (x : A) -> (y : A) -> A)
        : Type)|}

  let true_ =
    check "true"
      {|(((A : Type) => (x : A) => (y : A) => x)
        : (A : Type) -> (x : A) -> (y : A) -> A)|}

  let true_unify =
    check "true_unify"
      {|(((A : Type) => x => (y : A) => x)
        : (A : Type) -> (x : A) -> (y : A) -> A)|}

  let false_ =
    check "false"
      {|((A => (x : A) => (y : A) => y)
        : (A : Type) -> (x : A) -> (y : A) -> A)|}

  let ind_false_T =
    check "False_T"
      {|
        (@self(False -> (f : @self(f -> @unroll False f)) -> Type) : Type)
      |}

  let ind_false =
    check "False"
      {|
        (@fix(False => f =>
          (P : (f : @self(f -> @unroll False f)) -> Type) -> P f
        ) : @self(False -> (f : @self(f -> @unroll False f)) -> Type))
      |}

  let let_alias =
    check "let_alias"
      {|
        Id : (A : Type) -> Type = (A : Type) => A;
        ((A : Type) => (x : A) => (x : Id A))
      |}

  let simple_string = check "simple_string" {|("simple string" : String)|}

  let rank_2_propagate =
    check "rank_2_propagate"
      {|
        Unit = (A : Type) -> (x : A) -> A;
        (u => u Unit u : (u : Unit) -> Unit)
      |}

  let rank_2_propagate_let =
    check "rank_2_propagate"
      {|
        Unit = (A : Type) -> (x : A) -> A;
        noop : (u : Unit) -> Unit = u => u Unit u;
        noop
      |}

  let invalid_annotation = fail "invalid_annotation" {|(String : "A")|}
  let simplest_escape_check = fail "simplest_escape_check" "x => A => (x : A)"

  let bound_var_escape_check =
    fail "bound_var_escape_check"
      {|
        call = f => v => f v;
        (never : (A : Type) -> A) => call never
      |}

  let hole_lowering_check =
    fail "hole_lowering_check"
      {|
        x => (A : Type) => y => (id => (_ = (id x); _ = id y; (y : A))) (x => x)
      |}

  let nat_256_equality =
    check "nat_256_equality"
      {|
        Eq = (A : Type) => (x : A) => (y : A) =>
          (P : (z : A) -> Type) -> (l : P x) -> P y;
        refl : (A : Type) -> (x : A) -> Eq A x x
          = (A : Type) => (x : A) =>
            (P : (z : A) -> Type) => (l : P x) => l;

        Nat = (A : Type) -> (z : A) -> (s : (acc : A) -> A) -> A;
        zero : Nat = (A : Type) => (z : A) => (s : (acc : A) -> A) => z;
        succ : (pred : Nat) -> Nat = (pred : Nat) => 
          (A : Type) => (z : A) => (s : (acc : A) -> A) => s (pred A z s);
        one = succ zero;

        add = (a : Nat) => (b : Nat) => a Nat b succ;
        mul = (a : Nat) => (b : Nat) => a Nat zero ((n : Nat) => add n b);
        
        two = succ one;
        three = succ two;
        four = add two two;
        eight = add four four;
        sixteen = add eight eight;
        n256 = mul sixteen sixteen;
        sixteen_is_eight_times_two : Eq Nat sixteen (mul eight two)
          = refl Nat sixteen;
        (refl Nat n256 : Eq Nat (mul (mul eight eight) four) n256)
      |}

  let simple_alpha_rename =
    check "simple_alpha_rename"
      {|( (f : (B : Type) -> B) => (f ((C : Type) -> C) : (D : Type) -> D)
        : (f : (E : Type) -> E) -> (F : Type) -> F)|}

  let _tests =
    [
      id_propagate;
      id_unify;
      let_id;
      return_id_propagate;
      sequence;
      true_unify;
      false_;
      ind_false_T;
      ind_false;
      rank_2_propagate;
      rank_2_propagate_let;
      simplest_escape_check;
    ]

  let tests =
    [
      univ_type;
      string_type;
      false_type;
      id;
      (*
      id_propagate;
      id_unify;
      let_id;
      *)
      id_type;
      id_type_never;
      (* return_id_propagate; *)
      (* sequence; *)
      bool;
      true_;
      (* true_unify; *)
      (* false_; *)
      let_alias;
      simple_string;
      (*
      ind_false_T;
      ind_false;
      rank_2_propagate;
      rank_2_propagate_let;
      *)
      invalid_annotation;
      (* simplest_escape_check; *)
      bound_var_escape_check;
      hole_lowering_check;
      nat_256_equality;
      simple_alpha_rename;
    ]

  (* alcotest *)
  let test test =
    let check ~name ~annotated_term =
      Alcotest.test_case name `Quick @@ fun () ->
      let actual = Clexer.from_string Cparser.term_opt annotated_term in
      match actual with
      | Some stree -> (
          let ltree = Lparser.parse_term stree in
          match Typer.Infer.infer_term ltree with
          | Ok ttree -> Format.eprintf "%a\n%!" Tprinter.pp_term ttree
          | Error error ->
              failwith
              @@ Format.asprintf "error: %a\n%!" Tprinter.pp_error error)
      | None -> failwith "failed to parse"
    in
    let fail ~name ~annotated_term =
      Alcotest.test_case name `Quick @@ fun () ->
      let actual = Clexer.from_string Cparser.term_opt annotated_term in
      match actual with
      | Some stree -> (
          let ltree = Lparser.parse_term stree in
          match Typer.Infer.infer_term ltree with
          | Ok _ttree -> failwith "worked but should had failed"
          (* TODO: check for specific error *)
          | Error _error -> ())
      | None -> failwith "failed to parse"
    in
    match test with
    | Check { name; annotated_term } -> check ~name ~annotated_term
    | Fail { name; annotated_term } -> fail ~name ~annotated_term

  let tests = ("typer", List.map test tests)
end

let () = Alcotest.run "Teika" [ Typer.tests ]

(* TODO: (n : Nat & n >= 1, x : Nat) should be valid
   *)
