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
      ( "pair annot",
        works "(x : A, y : B)"
          (parens ((var "x" @: var "A") + (var "y" @: var "B"))) );
      ( "pair bind",
        works "(x = A, y = B)"
          (parens ((var "x" = var "A") + (var "y" = var "B"))) );
      ("both", works "(x & y & z)" (parens (var "x" & var "y" & var "z")));
      ( "both annot",
        works "(x : A & y : B)"
          (parens ((var "x" @: var "A") & (var "y" @: var "B"))) );
      ( "both bind",
        works "(x = A & y = B)" (parens (var "x" = var "A" & var "y" = var "B"))
      );
      ( "pair and both",
        works "(x : A & y : B, z : C)"
          (parens
             (((var "x" @: var "A") & (var "y" @: var "B"))
             + (var "z" @: var "C"))) );
      ("bind", works "x = y" (var "x" = var "y"));
      ("semi", works "x; y; z" (var "x" * (var "y" * var "z")));
      ("annot", works "(x : y)" (parens (var "x" @: var "y")));
      ( "nested annot",
        works "(x : A : B)" (parens (var "x" @: var "A" @: var "B")) );
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

module Ltree_utils = struct
  open Ltree

  module Location = struct
    include Location

    let equal _ _ = true
  end

  type term = Ltree.term = private
    | LTerm of { loc : Location.t; [@opaque] desc : term_desc }

  and term_desc = Ltree.term_desc = private
    | LT_var of { var : Name.t }
    | LT_forall of { param : annot; return : term }
    | LT_lambda of { param : annot; return : term }
    | LT_apply of { lambda : term; arg : term }
    | LT_exists of { left : annot; right : annot }
    | LT_pair of { left : bind; right : bind }
    | LT_unpair of { left : Name.t; right : Name.t; pair : term; return : term }
    | LT_either of { left : annot; right : annot }
    | LT_both of { left : bind; right : bind }
    | LT_unboth of { left : Name.t; right : Name.t; both : term; return : term }
    | LT_let of { bound : bind; return : term }
    | LT_annot of { value : term; type_ : term }

  and annot = Ltree.annot = private
    | LAnnot of { loc : Location.t; [@opaque] var : Name.t; type_ : term }

  and bind = Ltree.bind = private
    | LBind of { loc : Location.t; [@opaque] var : Name.t; value : term }
  [@@deriving eq]

  let loc = Location.none

  let var ?(loc = loc) var =
    let var = Name.make var in
    lt_var loc ~var

  let ( @-> ) ?(loc = loc) param return = lt_forall loc ~param ~return
  let ( @=> ) ?(loc = loc) param return = lt_lambda loc ~param ~return
  let ( @@ ) ?(loc = loc) lambda arg = lt_apply loc ~lambda ~arg
  let exists ?(loc = loc) left right = lt_exists loc ~left ~right
  let pair ?(loc = loc) left right = lt_pair loc ~left ~right

  let unpair ?(loc = loc) left right pair return =
    let left = Name.make left in
    let right = Name.make right in
    lt_unpair loc ~left ~right ~pair ~return

  let either ?(loc = loc) left right = lt_either loc ~left ~right
  let both ?(loc = loc) left right = lt_both loc ~left ~right

  let unboth ?(loc = loc) left right both return =
    let left = Name.make left in
    let right = Name.make right in
    lt_unboth loc ~left ~right ~both ~return

  let let_ ?(loc = loc) bound return = lt_let loc ~bound ~return
  let ( $ ) left right = left right
  let annot ?(loc = loc) value type_ = lt_annot loc ~value ~type_

  let ( @: ) ?(loc = loc) var type_ =
    let var = Name.make var in
    lannot loc ~var ~type_

  let ( @= ) ?(loc = loc) var value =
    let var = Name.make var in
    lbind loc ~var ~value
end

module Lparser = struct
  open Ltree_utils

  type test = Works of { expected : term; input : string }

  let works input expected = Works { expected; input }

  let tests =
    [
      ("variable", works "x" (var "x"));
      ("arrow", works "(x : A) -> x" (("x" @: var "A") @-> var "x"));
      ("lambda", works "(x : A) => x" (("x" @: var "A") @=> var "x"));
      ("apply", works "x y z" ((var "x" @@ var "y") @@ var "z"));
      ( "exists",
        works "(x : A, y : B)" (exists ("x" @: var "A") ("y" @: var "B")) );
      ("pair", works "(x = l, y = r)" (pair ("x" @= var "l") ("y" @= var "r")));
      ("unpair", works "(x, y) = p; z" (unpair "x" "y" (var "p") $ var "z"));
      ( "either",
        works "(x : A & y : B)" (either ("x" @: var "A") ("y" @: var "B")) );
      ("both", works "(x = l & y = r)" (both ("x" @= var "l") ("y" @= var "r")));
      ("unboth", works "(x & y) = p; z" (unboth "x" "y" (var "p") $ var "z"));
      ( "let",
        works "x = y; z = x; z"
          (let_ ("x" @= var "y") $ (let_ ("z" @= var "x") $ var "z")) );
      ("annot", works "(x : y)" (annot (var "x") (var "y")));
    ]

  (* alcotest *)
  let equal_stree = Alcotest.testable Ltree.pp_term Ltree_utils.equal_term

  let test (name, test) =
    let check () =
      let (Works { expected; input }) = test in
      let actual = Slexer.from_string Teika.Sparser.term_opt input in
      let actual =
        match actual with
        | Some stree -> Some (Lparser.from_stree stree)
        | None -> None
      in
      Alcotest.(
        check' (option equal_stree) ~msg:"works" ~expected:(Some expected)
          ~actual)
    in
    Alcotest.test_case name `Quick check

  let tests = ("lparser", List.map test tests)
end

let () = Alcotest.run "Teika" [ Sparser.tests; Lparser.tests ]

(* TODO: (n : Nat & n >= 1, x : Nat) should be valid
   *)
