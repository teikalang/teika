open Smol

type test = { name : string; term : string }

let type_term name term = { name; term }

let id =
  type_term "id"
    {|((A : Type) => (x : A) => x
      :(A : Type) -> (x : A) -> A)|}

(* let id_propagate =
   type_term "id_propagate" {|(A => x => x : (A : Type) -> (x : A) -> A)|} *)

let sequence =
  type_term "sequence"
    {|((A : Type) => (x : A) => (B : Type) => (y : B) => y
      :(A : Type) -> (x : A) -> (B : Type) -> (y : B) -> B)|}

let bool =
  type_term "bool"
    {|((A : Type) => (x : A) => (y : A) => x
               :(A : Type) -> (x : A) -> (y : A) -> A)|}

(* let sequence_propagate =
   type_term "sequence_propagate"
     {|(A => x => B => y => y
       :(A : Type) -> (x : A) -> (B : Type) -> (y : B) -> B)|} *)

let true_ =
  type_term "true"
    {|((A : Type) => (x : A) => (y : A) => x
      :(A : Type) -> (x : A) -> (y : A) -> A)|}

(* let true_propagate =
   type_term "true_propagate"
     {|(A => x => y => x
       :(A : Type) -> (x : A) -> (y : A) -> A)|} *)

let false_ =
  type_term "false"
    {|((A : Type) => (x : A) => (y : A) => y
      :(A : Type) -> (x : A) -> (y : A) -> A)|}

let tests =
  [
    id;
    (* id_propagate; *)
    sequence;
    (* sequence_propagate; *)
    bool;
    true_;
    (* true_propagate; *)
    false_;
  ]

let type_term term =
  let term = Slexer.from_string Sparser.term_opt term in
  let term = Option.get term in
  let term =
    let loc = Location.none in
    Lparser.parse_term ~loc term
  in
  let term =
    let open Ttyper in
    let ctx = Translate.Context.initial in
    Translate.translate_term ctx term
  in
  let ctx = Ttyper.Context.initial in
  Ttyper.infer_term ctx term

let test { name; term } =
  let check () =
    let _term = type_term term in
    ()
  in
  Alcotest.test_case name `Quick check

let tests = ("tests", List.map test tests)
let () = Alcotest.run "Typer" [ tests ]
