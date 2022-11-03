open Teika

module Stree_utils = struct
  open Stree

  module Location = struct
    include Location

    let equal _ _ = true
  end

  type term = Stree.term = private
    | ST of { loc : Location.t; desc : term_desc }

  and term_desc = Stree.term_desc = private
    | ST_var of { var : Name.t }
    | ST_forall of { param : term; return : term }
    | ST_lambda of { param : term; return : term }
    | ST_apply of { lambda : term; arg : term }
    | ST_pair of { left : term; right : term }
    | ST_both of { left : term; right : term }
    | ST_bind of { bound : term; value : term }
    | ST_semi of { left : term; right : term }
    | ST_annot of { value : term; type_ : term }
    | ST_parens of { content : term }
    | ST_braces of { content : term }
  [@@deriving eq]

  (* TODO: ppx? *)
  let loc = Location.none

  let var ?(loc = loc) var =
    let var = Name.make var in
    st_var loc ~var

  let ( @-> ) ?(loc = loc) param return = st_forall loc ~param ~return
  let ( @=> ) ?(loc = loc) param return = st_lambda loc ~param ~return
  let ( @@ ) ?(loc = loc) lambda arg = st_apply loc ~lambda ~arg
  let ( + ) ?(loc = loc) left right = st_pair loc ~left ~right
  let ( & ) ?(loc = loc) left right = st_both loc ~left ~right
  let ( = ) ?(loc = loc) bound value = st_bind loc ~bound ~value
  let ( * ) ?(loc = loc) left right = st_semi loc ~left ~right
  let ( @: ) ?(loc = loc) value type_ = st_annot loc ~value ~type_
  let parens ?(loc = loc) content = st_parens loc ~content
  let braces ?(loc = loc) content = st_braces loc ~content
end

module Sparser = struct
  open Stree_utils

  type test = Works of { expected : term; input : string }

  let works input expected = Works { expected; input }

  (* TODO: generative tests*)
  let tests =
    [
      ("variable", works "x" (var "x"));
      ("arrow", works "x -> x" (var "x" @-> var "x"));
      ("lambda", works "x => x" (var "x" @=> var "x"));
      ("apply", works "x y z" ((var "x" @@ var "y") @@ var "z"));
      ("pair", works "(x, y, z)" (parens (var "x" + (var "y" + var "z"))));
      ("both", works "(x & y & z)" (parens (var "x" & var "y" & var "z")));
      ("bind", works "x = y" (var "x" = var "y"));
      ("semi", works "x; y; z" (var "x" * (var "y" * var "z")));
      ("annot", works "(x : y)" (parens (var "x" @: var "y")));
      ("parens", works "(x)" (parens (var "x")));
      ("braces", works "{x}" (braces (var "x")));
      ( "let",
        works "x = y; z = x; z"
          ((var "x" = var "y") * ((var "z" = var "x") * var "z")) );
    ]

  (* alcotest *)
  let equal_stree = Alcotest.testable Stree.pp_term Stree_utils.equal_term

  let test (name, test) =
    let check () =
      let (Works { expected; input }) = test in
      let actual = Slexer.from_string Sparser.term_opt input in
      Alcotest.(
        check' (option equal_stree) ~msg:"works" ~expected:(Some expected)
          ~actual)
    in
    Alcotest.test_case name `Quick check

  let tests = ("sparser", List.map test tests)
end

let () = Alcotest.run "Teika" [ Sparser.tests ]
