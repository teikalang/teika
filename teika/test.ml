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
    | LT_forall of { param : pat; return : term }
    | LT_lambda of { param : pat; return : term }
    | LT_apply of { lambda : term; arg : term }
    | LT_exists of { left : annot; right : annot }
    | LT_pair of { left : bind; right : bind }
    | LT_let of { bound : bind; return : term }
    | LT_annot of { value : term; annot : term }

  and pat = Ltree.pat = private
    | LPat of { loc : Location.t; [@opaque] desc : pat_desc }

  and pat_desc = Ltree.pat_desc = private
    | LP_var of { var : Name.t }
    | LP_pair of { left : pat; right : pat }
    | LP_annot of { pat : pat; annot : term }

  and annot = Ltree.annot = private
    | LAnnot of { loc : Location.t; [@opaque] pat : pat; annot : term }

  and bind = Ltree.bind = private
    | LBind of { loc : Location.t; [@opaque] pat : pat; value : term }
  [@@deriving eq]

  let loc = Location.none

  let var ?(loc = loc) var =
    let var = Name.make var in
    lt_var loc ~var

  let pvar ?(loc = loc) var =
    let var = Name.make var in
    lp_var loc ~var

  let ppair ?(loc = loc) left right = lp_pair loc ~left ~right
  let ( $: ) ?(loc = loc) pat annot = lp_annot loc ~pat ~annot
  let ( @-> ) ?(loc = loc) param return = lt_forall loc ~param ~return
  let ( @=> ) ?(loc = loc) param return = lt_lambda loc ~param ~return
  let ( @@ ) ?(loc = loc) lambda arg = lt_apply loc ~lambda ~arg
  let exists ?(loc = loc) left right = lt_exists loc ~left ~right
  let pair ?(loc = loc) left right = lt_pair loc ~left ~right
  let let_ ?(loc = loc) bound return = lt_let loc ~bound ~return
  let ( $ ) left right = left right
  let annot ?(loc = loc) value annot = lt_annot loc ~value ~annot
  let ( @: ) ?(loc = loc) pat annot = lannot loc ~pat ~annot
  let ( @= ) ?(loc = loc) pat value = lbind loc ~pat ~value
end

module Lparser = struct
  open Teika
  open Ltree_utils

  type test = Works of { expected : term; input : string }

  let works input expected = Works { expected; input }

  let tests =
    [
      ("variable", works "x" (var "x"));
      ("arrow", works "(x : A) -> x" ((pvar "x" $: var "A") @-> var "x"));
      ("lambda", works "(x : A) => x" ((pvar "x" $: var "A") @=> var "x"));
      ("apply", works "x y z" ((var "x" @@ var "y") @@ var "z"));
      ( "exists",
        works "(x : A, y : B)"
          (exists (pvar "x" @: var "A") (pvar "y" @: var "B")) );
      ( "pair",
        works "(x = l, y = r)"
          (pair (pvar "x" @= var "l") (pvar "y" @= var "r")) );
      ( "unpair",
        works "(x, y) = p; z"
          (let_ (ppair (pvar "x") (pvar "y") @= var "p") $ var "z") );
      ( "let",
        works "x = y; z = x; z"
          (let_ (pvar "x" @= var "y") $ (let_ (pvar "z" @= var "x") $ var "z"))
      );
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
  module Typer_context = Context.Typer_context (Normalize) (Unify)

  type term = Ttree.term =
    | TTerm of { loc : Location.t; [@opaque] desc : term_desc; type_ : type_ }

  and type_ = Ttree.type_ =
    | TType of { loc : Location.t; [@opaque] desc : term_desc }

  and term_desc = Ttree.term_desc =
    | TT_var of { offset : Offset.t }
    | TT_forall of { param : pat; return : type_ }
    | TT_lambda of { param : pat; return : term }
    | TT_apply of { lambda : term; arg : term }
    | TT_exists of { left : annot; right : annot }
    | TT_pair of { left : bind; right : bind }
    | TT_let of { bound : bind; return : term }
    | TT_annot of { value : term; annot : type_ }
    | TT_offset of { desc : term_desc; offset : Offset.t }

  and pat = Ttree.pat =
    | TPat of { loc : Location.t; [@opaque] desc : pat_desc; type_ : type_ }

  and pat_desc = Ttree.pat_desc =
    | TP_var of { var : Name.t }
    | TP_pair of { left : pat; right : pat }
    | TP_annot of { pat : pat; annot : type_ }

  and annot = Ttree.annot = private
    | TAnnot of { loc : Location.t; [@opaque] pat : pat; annot : type_ }

  and bind = Ttree.bind = private
    | TBind of { loc : Location.t; [@opaque] pat : pat; value : term }
  [@@deriving show { with_path = false }]

  type error = Context.error = private
    | CError of { loc : Location.t; [@opaque] desc : error_desc }

  and error_desc = Context.error_desc = private
    (* typer *)
    | CError_typer_pat_not_annotated of { pat : Ltree.pat_desc }
    | CError_typer_pat_not_pair of { pat : Ltree.pat_desc; expected : type_ }
    (* unify *)
    | CError_unify_var_clash of { expected : Offset.t; received : Offset.t }
    | CError_unify_type_clash of { expected : term_desc; received : term_desc }
    | CError_unify_pat_clash of { expected : pat_desc; received : pat_desc }
    (* typer *)
    | CError_typer_unknown_var of { var : Name.t }
    | Cerror_typer_not_a_forall of { type_ : type_ }
    | Cerror_typer_not_an_exists of { type_ : type_ }
  [@@deriving show { with_path = false }]

  let infer_term term =
    let open Typer_context in
    let loc = Location.none in
    run ~loc @@ fun () -> Typer.infer_term term

  (* let normalize_term term =
       Context.Normalize_context.test ~loc:Location.none ~vars:[]
         ~offset:Offset.zero
       @@ fun () -> Normalize.normalize_term term

     let dump code =
       let stree = Slexer.from_string Sparser.term_opt code |> Option.get in
       let ltree = Lparser.from_stree stree in
       let ttree = infer_term ltree |> Result.get_ok in
       let ttree = normalize_term ttree |> Result.get_ok in
       let ttree = normalize_term ttree |> Result.get_ok in
       Format.eprintf "%a\n%!" pp_term ttree

     let () =
       dump "((id : (A : Type) -> (x : A) -> A) => id) (A => x => x) Type Type" *)
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

  let id_propagate =
    check "id_propagate" ~wrapper:false
      {|((A => x => x) : (A : Type) -> (x : A) -> A)|}

  let id_type =
    check "id_type" ~wrapper:false
      {|(((A : Type) => (x : A) => x) Type
        : (x : Type) -> Type)|}

  let id_type_never =
    check "id_type_never" ~wrapper:false
      {|(((A : Type) => (x : A) => x) Type ((A : Type) -> A)
        : Type)|}

  let return_id_propagate =
    check "return_id_propagate" ~wrapper:false
      {|((((id : (A : Type) -> (x : A) -> A) => id) (A => x => x))
        : (A : Type) -> (x : A) -> A)|}

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
      id;
      id_propagate;
      id_type;
      id_type_never;
      return_id_propagate;
      sequence;
      bool;
      true_;
      false_;
      pair;
      (* left_unpair;
         right_unpair; *)
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
