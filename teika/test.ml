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
    | ST_extension of { extension : Name.t }
    | ST_forall of { param : term; return : term }
    | ST_lambda of { param : term; return : term }
    | ST_apply of { lambda : term; arg : term }
    | ST_pair of { left : term; right : term }
    | ST_both of { left : term; right : term }
    | ST_bind of { bound : term; value : term }
    | ST_semi of { left : term; right : term }
    | ST_annot of { value : term; annot : term }
    | ST_string of { literal : string }
    | ST_parens of { content : term }
    | ST_braces of { content : term }
  [@@deriving eq]

  (* TODO: ppx? *)
  let loc = Location.none

  let var var =
    let var = Name.make var in
    st_var loc ~var

  let extension extension =
    let extension = Name.make extension in
    st_extension loc ~extension

  let ( @-> ) param return = st_forall loc ~param ~return
  let ( @=> ) param return = st_lambda loc ~param ~return
  let ( @@ ) lambda arg = st_apply loc ~lambda ~arg
  let ( + ) left right = st_pair loc ~left ~right
  let ( & ) left right = st_both loc ~left ~right
  let ( = ) bound value = st_bind loc ~bound ~value
  let ( * ) left right = st_semi loc ~left ~right
  let ( @: ) value annot = st_annot loc ~value ~annot
  let string literal = st_string loc ~literal
  let parens content = st_parens loc ~content
  let braces content = st_braces loc ~content
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
      ("extension", works "@fix" (extension "@fix"));
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
      ("string", works {|"hello"|} (string "hello"));
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

  let rec equal_term a b =
    match (a, b) with
    | LT_var { var = a }, LT_var { var = b } -> Name.equal a b
    | ( LT_extension { extension = a_extension; payload = a_payload },
        LT_extension { extension = b_extension; payload = b_payload } ) ->
        Name.equal a_extension b_extension && equal_term a_payload b_payload
    | ( LT_forall { param = a_param; return = a_return },
        LT_forall { param = b_param; return = b_return } ) ->
        equal_pat a_param b_param && equal_term a_return b_return
    | ( LT_lambda { param = a_param; return = a_return },
        LT_lambda { param = b_param; return = b_return } ) ->
        equal_pat a_param b_param && equal_term a_return b_return
    | ( LT_apply { lambda = a_lambda; arg = a_arg },
        LT_apply { lambda = b_lambda; arg = b_arg } ) ->
        equal_term a_lambda b_lambda && equal_term a_arg b_arg
    | ( LT_exists { left = a_left; right = a_right },
        LT_exists { left = b_left; right = b_right } ) ->
        equal_annot a_left b_left && equal_annot a_right b_right
    | ( LT_pair { left = a_left; right = a_right },
        LT_pair { left = b_left; right = b_right } ) ->
        equal_bind a_left b_left && equal_bind a_right b_right
    | ( LT_let { bound = a_bound; return = a_return },
        LT_let { bound = b_bound; return = b_return } ) ->
        equal_bind a_bound b_bound && equal_term a_return b_return
    | ( LT_annot { term = a_term; annot = a_annot },
        LT_annot { term = b_term; annot = b_annot } ) ->
        equal_term a_term b_term && equal_term a_annot b_annot
    | LT_string { literal = a }, LT_string { literal = b } -> String.equal a b
    (* TODO: loc equality *)
    | LT_loc { term = a; loc = _ }, b | a, LT_loc { term = b; loc = _ } ->
        equal_term a b
    | ( ( LT_var _ | LT_extension _ | LT_forall _ | LT_lambda _ | LT_apply _
        | LT_exists _ | LT_pair _ | LT_let _ | LT_annot _ | LT_string _ ),
        ( LT_var _ | LT_extension _ | LT_forall _ | LT_lambda _ | LT_apply _
        | LT_exists _ | LT_pair _ | LT_let _ | LT_annot _ | LT_string _ ) ) ->
        false

  and equal_pat a b =
    match (a, b) with
    | LP_var { var = a }, LP_var { var = b } -> Name.equal a b
    | ( LP_pair { left = a_left; right = a_right },
        LP_pair { left = b_left; right = b_right } ) ->
        equal_pat a_left b_left && equal_pat a_right b_right
    | ( LP_annot { pat = a_pat; annot = a_annot },
        LP_annot { pat = b_pat; annot = b_annot } ) ->
        equal_pat a_pat b_pat && equal_term a_annot b_annot
    | LP_loc { pat = a; loc = _ }, b | a, LP_loc { pat = b; loc = _ } ->
        (* TODO: loc equality *)
        equal_pat a b
    | (LP_var _ | LP_pair _ | LP_annot _), (LP_var _ | LP_pair _ | LP_annot _)
      ->
        false

  and equal_annot a b =
    let (LAnnot { loc = _; pat = a_pat; annot = a_annot }) = a in
    let (LAnnot { loc = _; pat = b_pat; annot = b_annot }) = b in
    equal_pat a_pat b_pat && equal_term a_annot b_annot

  and equal_bind a b =
    let (LBind { loc = _; pat = a_pat; value = a_value }) = a in
    let (LBind { loc = _; pat = b_pat; value = b_value }) = b in
    equal_pat a_pat b_pat && equal_term a_value b_value

  let var var =
    let var = Name.make var in
    LT_var { var }

  let ( @ ) extension payload =
    let extension = Name.make extension in
    LT_extension { extension; payload }

  let pvar var =
    let var = Name.make var in
    LP_var { var }

  let ppair left right = LP_pair { left; right }
  let ( $: ) pat annot = LP_annot { pat; annot }
  let ( @-> ) param return = LT_forall { param; return }
  let ( @=> ) param return = LT_lambda { param; return }
  let ( @@ ) lambda arg = LT_apply { lambda; arg }
  let exists left right = LT_exists { left; right }
  let pair left right = LT_pair { left; right }
  let let_ bound return = LT_let { bound; return }
  let ( $ ) left right = left right
  let annot term annot = LT_annot { term; annot }
  let string literal = LT_string { literal }
  let ( @: ) pat annot = LAnnot { loc = Location.none; pat; annot }
  let ( @= ) pat value = LBind { loc = Location.none; pat; value }
end

module Lparser = struct
  open Teika
  open Ltree
  open Ltree_utils

  type test = Works of { expected : term; input : string }

  let works input expected = Works { expected; input }

  let tests =
    [
      ("variable", works "x" (var "x"));
      ("extension", works "@fix(x)" ("@fix" @ var "x"));
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
      ("string", works {|"tuturu"|} (string "tuturu"));
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
  open Context

  let infer_term term = Typer_context.run @@ fun () -> Typer.infer_term term

  (* let dump code =
       let stree = Slexer.from_string Sparser.term_opt code |> Option.get in
       let ltree = Lparser.from_stree stree in
       let ttree =
         match infer_term ltree with
         | Ok ttree -> ttree
         | Error error ->
             Format.eprintf "%a\n%!" Context.pp_error error;
             failwith "infer"
       in
       let (TT_typed { term = _ttree; annot = ttree }) = ttree in
       Format.eprintf "%a\n%!" Tprinter.pp_term ttree;
       assert false

     let () = dump {|(id = (A : Type) => (x : A) => x; id)|} *)
end

module Typer = struct
  open Teika
  open Ttree_utils

  type test =
    | Check of { name : string; annotated_term : string }
    | Fail of { name : string; annotated_term : string }

  let check name annotated_term = Check { name; annotated_term }
  let fail name annotated_term = Fail { name; annotated_term }

  (* TODO: write tests for locations and names / offset *)
  (* TODO: write tests for escape check *)
  let univ_type = check "Type" {|(Type : Type)|}
  let string_type = check "String" {|(String : Type)|}

  let id =
    check "id" {|((A : Type) => (x : A) => x : (A : Type) -> (x : A) -> A)|}

  let id_propagate =
    check "id_propagate" {|((A => x => x) : (A : Type) -> (x : A) -> A)|}

  let id_unify =
    check "id_unify" {|((A => (x : A) => x) : (A : Type) -> (x : A) -> A)|}

  let let_id =
    check "let_id"
      {|((id = A => (x : A) => x; id) : (A : Type) -> (x : A) -> A)|}

  let id_type =
    check "id_type" {|((A => (x : A) => x) Type
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

  let unfold_false =
    let ind_false =
      {|
        (@fix(False => f =>
          (P : (f : @self(f -> @unroll False f)) -> Type) -> P f
        ) : @self(False -> (f : @self(f -> @unroll False f)) -> Type))
      |}
    in
    let code =
      Format.sprintf
        {|((P => x => @unfold x) : 
          (P : (False : (f : @self(f -> @unroll (%s) f)) -> Type) -> Type) ->
          (x : P (@unroll (%s))) -> P ((f : @self(f -> @unroll (%s) f)) =>
              (P : (f : @self(f -> @unroll (%s) f)) -> Type) -> P f))|}
        ind_false ind_false ind_false ind_false
    in
    check "unfold False" code

  let let_alias =
    check "let_alias"
      {|
        Id = (A : Type) => A;
        (A => (x : A) => (x : Id A))
      |}

  let simple_string = check "simple_string" {|("simple string" : String)|}
  let invalid_annotation = fail "invalid_annotation" {|(String : "A")|}
  let simplest_escape_check = fail "simplest_escape_check" "x => A => (x : A)"

  let tests =
    [
      univ_type;
      string_type;
      id;
      id_propagate;
      id_unify;
      let_id;
      id_type;
      id_type_never;
      return_id_propagate;
      sequence;
      bool;
      true_;
      true_unify;
      false_;
      ind_false_T;
      ind_false;
      unfold_false;
      let_alias;
      simple_string;
      invalid_annotation;
      simplest_escape_check;
    ]

  (* alcotest *)
  let test test =
    let check ~name ~annotated_term =
      Alcotest.test_case name `Quick @@ fun () ->
      let actual = Slexer.from_string Sparser.term_opt annotated_term in
      match actual with
      | Some stree -> (
          let ltree = Lparser.from_stree stree in
          match infer_term ltree with
          | Ok _ttree ->
              Format.eprintf "%a\n%!" Tprinter.pp_term _ttree;
              ()
          | Error error ->
              failwith @@ Format.asprintf "error: %a\n%!" Terror.pp error)
      | None -> failwith "failed to parse"
    in
    let fail ~name ~annotated_term =
      Alcotest.test_case name `Quick @@ fun () ->
      let actual = Slexer.from_string Sparser.term_opt annotated_term in
      match actual with
      | Some stree -> (
          let ltree = Lparser.from_stree stree in
          match infer_term ltree with
          | Ok _ttree -> failwith "failed but should had worked "
          (* TODO: check for specific error *)
          | Error _error -> ())
      | None -> failwith "failed to parse"
    in
    match test with
    | Check { name; annotated_term } -> check ~name ~annotated_term
    | Fail { name; annotated_term } -> fail ~name ~annotated_term

  let tests = ("typer", List.map test tests)
end

let () = Alcotest.run "Teika" [ Sparser.tests; Lparser.tests; Typer.tests ]

(* TODO: (n : Nat & n >= 1, x : Nat) should be valid
   *)
