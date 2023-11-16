open Syntax
open Smol

type test = { name : string; term : string }

let type_term name term = { name; term }

let id =
  type_term "id"
    {|((A : Type) => (x : A) => x
      :(A : Type) -> (x : A) -> A)|}

let id_propagate =
  type_term "id_propagate" {|(A => x => x : (A : Type) -> (x : A) -> A)|}

let sequence =
  type_term "sequence"
    {|((A : Type) => (x : A) => (B : Type) => (y : B) => y
      :(A : Type) -> (x : A) -> (B : Type) -> (y : B) -> B)|}

let bool =
  type_term "bool"
    {|((A : Type) => (x : A) => (y : A) => x
               :(A : Type) -> (x : A) -> (y : A) -> A)|}

let sequence_propagate =
  type_term "sequence_propagate"
    {|(A => x => B => y => y
       :(A : Type) -> (x : A) -> (B : Type) -> (y : B) -> B)|}

let true_ =
  type_term "true"
    {|((A : Type) => (x : A) => (y : A) => x
      :(A : Type) -> (x : A) -> (y : A) -> A)|}

let true_propagate =
  type_term "true_propagate"
    {|(A => x => y => x
       :(A : Type) -> (x : A) -> (y : A) -> A)|}

let false_ =
  type_term "false"
    {|((A : Type) => (x : A) => (y : A) => y
      :(A : Type) -> (x : A) -> (y : A) -> A)|}

let ind_False =
  let b_false = {|f @-> (P : (f : @False I_False) -> Type) -> @I_False P f|} in
  let i_false_t =
    Format.sprintf
      {|I_False @-> (P : (f : @False I_False) -> Type) -> (f : %s) -> Type|}
      b_false
  in
  let code =
    Format.sprintf
      {|
        (FalseT : Type) === False @-> (I_False : %s) -> Type;
        (False : FalseT) @=> (I_False : %s) => %s
      |}
      i_false_t i_false_t b_false
  in
  type_term "ind_False" code

let ind_Unit =
  let b_Unit =
    {|
      u @-> (P : (x : @Unit I_Unit unit I_unit) -> Type) ->
        (x : @I_Unit unit I_unit P (@unit I_unit)) -> @I_Unit unit I_unit P u
    |}
  in
  let b_unit =
    {|
      (u : u @->
        (P : (x : @Unit I_Unit unit I_unit) -> Type) ->
        (a : @I_Unit unit I_unit P (@unit I_unit)) -> @I_Unit unit I_unit P u
      ) @=> (P : (x : @Unit I_Unit unit I_unit) -> Type) =>
        (b : @I_Unit unit I_unit P (@unit I_unit)) => @I_unit P b
    |}
  in
  let t_i_unit =
    Format.sprintf
      {|
        I_unit @-> (P : (c : @Unit I_Unit unit I_unit) -> Type) ->
          (d : @I_Unit unit I_unit P (@unit I_unit)) ->
          @I_Unit unit I_unit P (%s)
      |}
      b_unit
  in
  let t_unit =
    Format.sprintf {|
      unit @-> (I_unit : %s) -> %s
    |} t_i_unit b_Unit
  in
  let t_i_Unit =
    Format.sprintf
      {|
        I_Unit @-> (unit : %s) -> (I_unit : %s) ->
          (P : (e : @Unit I_Unit unit I_unit) -> Type) ->
          (f : %s) -> Type
      |}
      t_unit t_i_unit b_Unit
  in
  (*
  {|
    (UnitT : Type) ===
      Unit @-> (I_Unit : %s) -> (unit : %s) -> (I_unit : %s) -> Type;
    (Unit : UnitT) === (Unit : UnitT) @=> 
      (I_Unit : %s) => (unit : %s) => (I_unit : %s) => %s;
    (UnitR : Type) === (I_Unit : %s) -> (unit : %s) -> (I_unit : %s) -> Type;
    (UnitEq : (P : (x : UnitR) -> Type) -> (x : P @Unit) ->
      P ((I_Unit : %s) => (unit : %s) => (I_unit : %s) => %s)
    ) === (P : (x : UnitR) -> Type) => (x : P @Unit) => %%expand x;
    (I_Unit : %s) === (I_Unit : %s) @=> (unit : %s) => (I_unit : %s) =>
      (P : (f : @Unit I_Unit unit I_unit) -> Type) =>
        UnitEq ((Unit : UnitR) => (f : Unit I_Unit unit I_unit) -> Type) P;
    (unit : %s) === (unit : %s) @=> (I_unit : %s) => %s;
    (I_unitT : Type) === %s;
    (unitR : Type) === (I_unit : I_unitT) -> %s;
    (unitEq : (P : (x : unitR) -> Type) -> (x : P @unit) ->
      P ((I_unit : I_unitT) => %s)) ===
      (P : (x : unitR) -> Type) => (x : P @unit) => %%expand P;
    unitEq
  |}
  *)
  let _code =
    Format.sprintf
      {|
        (UnitT : Type) ===
          Unit @-> (I_Unit : %s) -> (unit : %s) -> (I_unit : %s) -> Type;
        (I_UnitT : (Unit : UnitT) -> Type) ===
          (Unit : UnitT) => %s;
        (unitT : (Unit : UnitT) -> (I_Unit : I_UnitT Unit) -> Type) ===
          (Unit : UnitT) => (I_Unit : I_UnitT Unit) => %s;
        (I_unitT : (Unit : UnitT) -> (I_Unit : I_UnitT Unit) ->
          (unit : unitT Unit I_Unit) -> Type) ===
          (Unit : UnitT) => (I_Unit : I_UnitT Unit) =>
          (unit : unitT Unit I_Unit) => %s;
        (Unit : UnitT) === (Unit : UnitT) @=> 
          (I_Unit : I_UnitT Unit) => (unit : unitT Unit I_Unit) =>
            (I_unit : I_unitT Unit I_Unit unit) => %s;
        (I_UnitT : Type) === I_UnitT Unit;
        (unitT : (I_Unit : I_UnitT) -> Type) === unitT Unit;
        (I_unitT : (I_Unit : I_UnitT) ->
          (unit : unitT I_Unit) -> Type) === I_unitT Unit;
        (UnitR : Type) === (I_Unit : I_UnitT) -> (unit : unitT I_Unit) ->
            (I_unit : I_unitT I_Unit unit) -> Type;
        (UnitEq : (P : (x : UnitR) -> Type) -> (x : P @Unit) ->
          P (
          (I_Unit : I_UnitT) => (unit : unitT I_Unit) =>
            (I_unit : I_unitT I_Unit unit) => %s)
        ) === (P : (x : UnitR) -> Type) => (x : P @Unit) => %%expand x;
        (I_Unit : I_UnitT) === (I_Unit : I_UnitT) @=>
          (unit : unitT I_Unit) => (I_unit : I_unitT I_Unit unit) =>
          (P : (f : @Unit I_Unit unit I_unit) -> Type) =>
            UnitEq ((Unit : UnitR) => (f : Unit I_Unit unit I_unit) -> Type) P;
        (unitT : Type) === unitT I_Unit;
        (I_unitT : (unit : unitT) -> Type) === I_unitT I_Unit;
        (unit : unitT) === (unit : unitT) @=>
          (I_unit : I_unitT unit) => %s;
        (I_unitT : Type) === I_unitT unit;
        (unitR : Type) === (I_unit : I_unitT) -> %s;
        (unitEq : (P : (x : unitR) -> Type) -> (x : P @unit) ->
          P ((I_unit : I_unitT) => %s)) ===
            (P : (x : unitR) -> Type) => (x : P @unit) => %%expand x;
        (I_unit : I_unitT) === (I_unit : I_unitT) @=>
          (P : (c : @Unit I_Unit unit I_unit) -> Type) =>
          (d : @I_Unit unit I_unit P (@unit I_unit)) =>
          unitEq ((at_unit : unitR) => @I_Unit unit I_unit P (at_unit I_unit))
            d;
        (UnitEq : (P : (_ : Type) -> Type) ->
          (x : P (@Unit I_Unit unit I_unit)) -> P (%s)) ===
          (P : (_ : Type) -> Type) =>
          (x : P (@Unit I_Unit unit I_unit)) => %%expand x;
        (RevUnitEq : (P : (_ : Type) -> Type) ->
          (x : P (%s)) -> P (@Unit I_Unit unit I_unit)) ===
          (P : (_ : Type) -> Type) =>
          UnitEq ((T : Type) => (x : P T) -> P (@Unit I_Unit unit I_unit))
            ((x : P (@Unit I_Unit unit I_unit)) => x);
        
        (unitK : @Unit I_Unit unit I_unit) === RevUnitEq ((X : Type) => X) (@unit I_unit);
        @(%%expand unitK)
      |}
      t_i_Unit t_unit t_i_unit t_i_Unit t_unit t_i_unit b_Unit b_Unit b_unit
      b_Unit b_unit b_Unit b_Unit
  in
  let code =
    Format.sprintf
      {|
        (UnitT : Type) ===
          Unit @-> (I_Unit : %s) -> (unit : %s) -> (I_unit : %s) -> Type;
        (I_UnitT : (Unit : UnitT) -> Type) ===
          (Unit : UnitT) => %s;
        (unitT : (Unit : UnitT) -> (I_Unit : I_UnitT Unit) -> Type) ===
          (Unit : UnitT) => (I_Unit : I_UnitT Unit) => %s;
        (I_unitT : (Unit : UnitT) -> (I_Unit : I_UnitT Unit) ->
          (unit : unitT Unit I_Unit) -> Type) ===
          (Unit : UnitT) => (I_Unit : I_UnitT Unit) =>
          (unit : unitT Unit I_Unit) => %s;
        (Unit : UnitT) === (Unit : UnitT) @=> 
          (I_Unit : I_UnitT Unit) => (unit : unitT Unit I_Unit) =>
            (I_unit : I_unitT Unit I_Unit unit) => %s;
        (I_UnitT : Type) === I_UnitT Unit;
        (unitT : (I_Unit : I_UnitT) -> Type) === unitT Unit;
        (I_unitT : (I_Unit : I_UnitT) ->
          (unit : unitT I_Unit) -> Type) === I_unitT Unit;
        (UnitR : Type) === (I_Unit : I_UnitT) -> (unit : unitT I_Unit) ->
            (I_unit : I_unitT I_Unit unit) -> Type;
        (UnitEq : (P : (x : UnitR) -> Type) -> (x : P @Unit) ->
          P (
          (I_Unit : I_UnitT) => (unit : unitT I_Unit) =>
            (I_unit : I_unitT I_Unit unit) => %s)
        ) === (P : (x : UnitR) -> Type) => (x : P @Unit) => %%expand x;
        (I_Unit : I_UnitT) === (I_Unit : I_UnitT) @=>
          (unit : unitT I_Unit) => (I_unit : I_unitT I_Unit unit) =>
          (P : (f : @Unit I_Unit unit I_unit) -> Type) =>
            UnitEq ((Unit : UnitR) => (f : Unit I_Unit unit I_unit) -> Type) P;
        I_Unit
      |}
      t_i_Unit t_unit t_i_unit t_i_Unit t_unit t_i_unit b_Unit b_Unit
  in
  type_term "ind_Unit" code

let tests =
  [
    id;
    id_propagate;
    sequence;
    sequence_propagate;
    bool;
    true_;
    true_propagate;
    false_;
    ind_False;
    ind_Unit;
  ]

let type_term term =
  let term = Clexer.from_string Cparser.term_opt term in
  let term = Option.get term in
  let term =
    let loc = Location.none in
    Lparser.parse_term ~loc term
  in
  Styper.(Context.run @@ fun () -> infer_term term)

let test { name; term } =
  let check () =
    let _type_ = type_term term in
    (* Format.eprintf "type_ : %a\n%!" Tprinter.pp_term type_; *)
    ()
  in
  Alcotest.test_case name `Quick check

let tests = ("tests", List.map test tests)
let () = Alcotest.run "Typer" [ tests ]
