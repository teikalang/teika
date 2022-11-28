module Stree_utils = struct
  open Teika
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
    | ST_annot of { value : term; annot : term }
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
  let ( @: ) ?(loc = loc) value annot = st_annot loc ~value ~annot
  let parens ?(loc = loc) content = st_parens loc ~content
  let braces ?(loc = loc) content = st_braces loc ~content
end

module Sparser = struct
  open Teika
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
  open Teika
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
    | LT_let of { bound : bind; return : term }
    | LT_annot of { value : term; annot : term }

  and annot = Ltree.annot = private
    | LAnnot of { loc : Location.t; [@opaque] var : Name.t; annot : term }

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

  let let_ ?(loc = loc) bound return = lt_let loc ~bound ~return
  let ( $ ) left right = left right
  let annot ?(loc = loc) value annot = lt_annot loc ~value ~annot

  let ( @: ) ?(loc = loc) var annot =
    let var = Name.make var in
    lannot loc ~var ~annot

  let ( @= ) ?(loc = loc) var value =
    let var = Name.make var in
    lbind loc ~var ~value
end

module Lparser = struct
  open Teika
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

module Ttree_utils = struct
  open Teika
  module Typer_context = Context.Typer_context (Instance) (Normalize) (Unify)

  type term = Ttree.term =
    | TTerm of { loc : Location.t; [@opaque] desc : term_desc; type_ : type_ }

  and type_ = Ttree.type_ =
    | TType of { loc : Location.t; [@opaque] desc : term_desc }

  and term_desc = Ttree.term_desc =
    (* x *)
    | TT_var of { offset : Offset.t }
    (* (x : A) -> B *)
    | TT_forall of { param : annot; return : type_ }
    (* (x : A) => e *)
    | TT_lambda of { param : annot; return : term }
    (* l a *)
    | TT_apply of { lambda : term; arg : term }
    (* (x : A, y : B) *)
    | TT_exists of { left : annot; right : annot }
    (* (x = 0, y = 0) *)
    | TT_pair of { left : bind; right : bind }
    (* (x, y) = v; r *)
    | TT_unpair of { left : Name.t; right : Name.t; pair : term; return : term }
    (* x = v; r *)
    | TT_let of { bound : bind; return : term }
    (* v : T *)
    | TT_annot of { value : term; annot : type_ }

  and annot = Ttree.annot = private
    | TAnnot of { loc : Location.t; [@opaque] var : Name.t; annot : type_ }

  and bind = Ttree.bind = private
    | TBind of { loc : Location.t; [@opaque] var : Name.t; value : term }
  [@@deriving show]

  type error = Context.error = private
    | CError of { loc : Location.t; [@opaque] desc : error_desc }

  and error_desc = Context.error_desc = private
    (* unify *)
    | CError_unify_var_clash of { expected : Offset.t; received : Offset.t }
    | CError_unify_type_clash of { expected : term_desc; received : term_desc }
    (* typer *)
    | CError_typer_unknown_var of { var : Name.t }
    | CError_typer_term_not_a_type of { term : term }
    | Cerror_typer_not_a_forall of { type_ : type_ }
    | Cerror_typer_not_an_exists of { type_ : type_ }
  [@@deriving show]

  let infer_term term =
    let open Typer_context in
    let loc = Location.none in
    run ~loc @@ fun () -> Typer.infer_term term
end

module Typer = struct
  open Teika
  open Ttree_utils

  type test =
    | Check of { name : string; annotated_term : string; wrapper : bool }

  let check ?(wrapper = true) name annotated_term =
    Check { name; annotated_term; wrapper }

  (* TODO: write tests for locations and names / offset *)
  let id =
    check "id" ~wrapper:false
      {|(((A : Type) => (x : A) => x)
        : (A : Type) -> (x : A) -> A)|}

  let id_type =
    check "id_type" ~wrapper:false
      {|(((A : Type) => (x : A) => x) Type
        : (x : Type) -> Type)|}

  let id_type_never =
    check "id_type_never" ~wrapper:false
      {|(((A : Type) => (x : A) => x) Type ((A : Type) -> A)
        : Type)|}

  let sequence =
    check "sequence" ~wrapper:false
      {|(((A : Type) => (x : A) => (B : Type) => (y : B) => y)
        : (A : Type) -> (x : A) -> (B : Type) -> (y : B) -> B)|}

  let bool =
    check "bool" ~wrapper:false
      {|(((A : Type) => (x : A) => (y : A) => x)
        : (A : Type) -> (x : A) -> (y : A) -> A)|}

  let true_ =
    check "true" ~wrapper:false
      {|(((A : Type) => (x : A) => (y : A) => x)
        : (A : Type) -> (x : A) -> (y : A) -> A)|}

  let false_ =
    check "false" ~wrapper:false
      {|(((A : Type) => (x : A) => (y : A) => y)
        : (A : Type) -> (x : A) -> (y : A) -> A)|}

  let pair =
    check "pair" ~wrapper:false
      {|(((A : Type) => (B : Type) => (x : A) => (y : B) => (x = x, y = y))
        : (A : Type) -> (B : Type) -> (x : A) -> (y : B) -> (x  : A, y : B))|}

  (* let left_unpair =
       check "left_unpair" ~wrapper:false
         {|(((A : Type) => (B : Type) => (x : A) => (y : B) => (
             p = (x = x, y = y);
             (y, x) = p;
             y
           )) : (A : Type) -> (B : Type) -> (x : A) -> (y : B) -> A)|}

     let right_unpair =
       check "right_unpair" ~wrapper:false
         {|(A : Type) => (B : Type) => (x : A) => (y : B) => (
             p = (x = x, y = y);
             (y, x) = p;
             x
           )) : (A : Type) -> (B : Type) -> (x : A) -> (y : B) -> B)|} *)

  (* TODO: something like pack *)
  (* let pack =
     type_expr "pack" ~wrapper:false
       ~type_:"(R: Type) -> (A: Type, r: R) -> (A: Type, r: R)"
       ~expr:"(R: Type) => (p: (A: Type, x: R)) => p" *)

  let tests =
    [
      (* unwrapped *)
      id;
      sequence;
      bool;
      true_;
      false_;
      pair;
      (* left_unpair;
         right_unpair; *)
      (* apply *)
      id_type;
      id_type_never;
    ]

  (* alcotest *)
  let test test =
    let (Check { name; annotated_term; wrapper = _ }) = test in
    let check () =
      let actual = Slexer.from_string Sparser.term_opt annotated_term in
      match actual with
      | Some stree -> (
          let ltree = Lparser.from_stree stree in
          match infer_term ltree with
          | Ok _ttree -> ()
          | Error error ->
              failwith @@ Format.asprintf "error: %a\n%!" pp_error error)
      | None -> failwith "failed to parse"
    in
    Alcotest.test_case name `Quick check

  let tests = ("typer", List.map test tests)
end

let () = Alcotest.run "Teika" [ Sparser.tests; Lparser.tests; Typer.tests ]

(* TODO: (n : Nat & n >= 1, x : Nat) should be valid
   *)
