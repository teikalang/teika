open Smol

type test = { name : string; term : string }

let type_term name term = { name; term }

let id =
  type_term "id"
    {|((A : Type) => (x : A) => x
      :(A : Type) -> (x : A) -> A)|}

let sequence =
  type_term "sequence"
    {|((A : Type) => (x : A) => (B : Type) => (y : B) => y
      :(A : Type) -> (x : A) -> (B : Type) -> (y : B) -> B)|}

let bool =
  type_term "bool"
    {|((A : Type) => (x : A) => (y : A) => x
         :(A : Type) -> (x : A) -> (y : A) -> A)|}

let true_ =
  type_term "true"
    {|((A : Type) => (x : A) => (y : A) => x
         :(A : Type) -> (x : A) -> (y : A) -> A)|}

let false_ =
  type_term "false"
    {|((A : Type) => (x : A) => (y : A) => y
         :(A : Type) -> (x : A) -> (y : A) -> A)|}

let tests = [ id; sequence; bool; true_; false_ ]

let type_term term =
  let term = Slexer.from_string Sparser.term_opt term in
  let term = Option.get term in
  let term =
    let loc = Location.none in
    Lparser.parse_term ~loc term
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
