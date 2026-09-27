open Lsp.Types

let uri = DocumentUri.of_path "/tmp/demo.teika"
let language_id = LanguageKind.Other "teika"
let position line character = Position.create ~line ~character

let range start_line start_character end_line end_character =
  Range.create
    ~start:(position start_line start_character)
    ~end_:(position end_line end_character)

let request id typed =
  Jsonrpc.Packet.Request
    (Lsp.Client_request.to_jsonrpc_request typed ~id:(`Int id))

let notification typed =
  Jsonrpc.Packet.Notification (Lsp.Client_notification.to_jsonrpc typed)

let response_json = function
  | [ Jsonrpc.Packet.Response { result = Ok json; _ } ] -> json
  | _ -> Alcotest.fail "expected exactly one successful response"

let initialize server =
  let capabilities = ClientCapabilities.create () in
  let params = InitializeParams.create ~capabilities () in
  Demo_language.Server.handle server
    (request 1 (Lsp.Client_request.Initialize params))
  |> fst |> response_json

let open_document server ~version text =
  let textDocument =
    TextDocumentItem.create ~uri ~languageId:language_id ~version ~text
  in
  Demo_language.Server.handle server
    (notification
       (Lsp.Client_notification.TextDocumentDidOpen
          (DidOpenTextDocumentParams.create ~textDocument)))

let diagnostics_of_packets = function
  | [ Jsonrpc.Packet.Notification notification ] ->
      Jsonrpc.Notification.yojson_of_t notification
      |> Yojson.Safe.Util.member "params"
      |> Yojson.Safe.Util.member "diagnostics"
      |> Yojson.Safe.Util.to_list
  | _ -> Alcotest.fail "expected one diagnostics notification"

let contains text substring =
  let text_length = String.length text in
  let substring_length = String.length substring in
  let rec search offset =
    if offset + substring_length > text_length then false
    else if String.sub text offset substring_length = substring then true
    else search (offset + 1)
  in
  search 0

let hover server ~id ~line ~character =
  let textDocument = TextDocumentIdentifier.create ~uri in
  let params =
    HoverParams.create ~textDocument ~position:(position line character) ()
  in
  Demo_language.Server.handle server
    (request id (Lsp.Client_request.TextDocumentHover params))
  |> fst |> response_json

let hover_contents json =
  Yojson.Safe.Util.member "contents" json
  |> Yojson.Safe.Util.member "value"
  |> Yojson.Safe.Util.to_string

let test_parser_diagnostic () =
  let server = Demo_language.Server.create () in
  ignore (initialize server);
  let packets, _ = open_document server ~version:1 "(" in
  let diagnostics = diagnostics_of_packets packets in
  Alcotest.(check int) "one parser diagnostic" 1 (List.length diagnostics);
  Alcotest.(check string)
    "parser message" "syntax error"
    (List.hd diagnostics
    |> Yojson.Safe.Util.member "message"
    |> Yojson.Safe.Util.to_string)

let test_lexer () =
  match Lsp_demo_syntax.Demo_lexer.tokens_from_string "hello" with
  | [ Lsp_demo_syntax.Cparser.VAR "hello"; EOF ] -> ()
  | _ -> Alcotest.fail "expected VAR followed by EOF"

let test_typer_diagnostic () =
  let server = Demo_language.Server.create () in
  ignore (initialize server);
  let packets, _ = open_document server ~version:1 "unbound" in
  let diagnostics = diagnostics_of_packets packets in
  Alcotest.(check int) "one typer diagnostic" 1 (List.length diagnostics);
  let message =
    List.hd diagnostics
    |> Yojson.Safe.Util.member "message"
    |> Yojson.Safe.Util.to_string
  in
  if not (contains message "unbound variable") then
    Alcotest.failf "unexpected typer diagnostic: %S" message

let test_bool_reaches_vendored_typer () =
  let module Analysis = Demo_language.Analysis in
  let analysis = Analysis.run "Bool = [true, false]; Bool" in
  match Analysis.issues analysis with
  | issue :: _ ->
      let phase = Alcotest.testable Analysis.pp_phase Analysis.equal_phase in
      Alcotest.check phase "failure phase" Analysis.Typing
        (Analysis.issue_phase issue)
  | [] -> Alcotest.fail "expected the vendored typer to reject unbound true"

let test_type_example_type_checks () =
  let module Analysis = Demo_language.Analysis in
  let analysis = Analysis.run "Type" in
  Alcotest.(check int) "no issues" 0 (List.length (Analysis.issues analysis))

let test_documented_core_type_checks () =
  let module Analysis = Demo_language.Analysis in
  let check source =
    let analysis = Analysis.run source in
    match Analysis.issues analysis with
    | [] -> ()
    | issue :: _ ->
        Alcotest.failf "%S failed during %a: %s" source Analysis.pp_phase
          (Analysis.issue_phase issue)
          (Analysis.issue_message issue)
  in
  List.iter check
    [
      "(A : Type) -> A";
      "((A : Type) => A) : (A : Type) -> Type";
      "Type : Type";
      "self(x) -> Type";
      "(fix(x) => Type) : self(x) -> Type";
      "((fix(x) => Type) : self(x) -> Type).@";
      "()";
      "[]";
      "() = (); Type";
      "[Type, Type]";
      "[A : Type, A]";
      "[Type, Type].0";
      "id(A : Type, x : A) : A; id(A, x) = x; id(Type, Type)";
    ]

let lower source =
  match Lsp_demo_syntax.Demo_lexer.term_opt_from_string source with
  | Some concrete -> Lsp_demo_syntax.Lparser.parse_term concrete
  | None -> Alcotest.failf "%S unexpectedly parsed as an empty document" source

let test_syntax_printer () =
  let check source expected =
    Alcotest.(check string)
      source expected
      (Demo_language.Printer.to_string (lower source))
  in
  check "f (g x)" "f(g(x))";
  check "f(left, right)" "f(left)(right)";
  check "[left,right]" "[left, right]";
  check "x=[one,two];x" "x = [one, two]; x";
  check "x:Type;x" "x : Type; x";
  check "value:Type:Type" "value : Type : Type";
  check "x:Type=value;x" "x : Type = value; x";
  check "(A : Type) -> A" "(A : Type) -> A";
  check "(A : Type) => A" "(A : Type) => A";
  check "self(x) -> Type" "self(x) -> Type";
  check "fix(x) => x" "fix(x) => x";
  check "value.@.0" "value.@.0";
  check "()=();Type" "() = (); Type";
  check "[a : Type, a]" "[a : Type, a]"

let test_printer_inserts_parentheses () =
  let check label expected term =
    Alcotest.(check string)
      label expected
      (Demo_language.Printer.to_string term)
  in
  check "annotated function" "((A : Type) => A) : (A : Type) -> Type"
    (lower "((A : Type) => A) : (A : Type) -> Type");
  check "application function" "((A : Type) => A)(Type)"
    (lower "((A : Type) => A)(Type)")

let test_ast_hover () =
  let server = Demo_language.Server.create () in
  ignore (initialize server);
  ignore (open_document server ~version:1 "hello");
  let contents = hover server ~id:2 ~line:0 ~character:1 |> hover_contents in
  Alcotest.(check bool) "node kind" true (contains contents "variable");
  Alcotest.(check bool) "node text" true (contains contents "\nhello\n");
  Alcotest.(check bool)
    "no concrete tree dump" false
    (contains contents "CT_var")

let test_documented_syntax_hover () =
  let server = Demo_language.Server.create () in
  ignore (initialize server);
  ignore (open_document server ~version:1 "self(x) -> Type");
  let contents = hover server ~id:2 ~line:0 ~character:1 |> hover_contents in
  Alcotest.(check bool) "node kind" true (contains contents "self type");
  Alcotest.(check bool)
    "lowered syntax" true
    (contains contents "\nself(x) -> Type\n");
  Alcotest.(check bool)
    "no concrete constructor" false (contains contents "CT_")

let test_sequence_body_hover () =
  let server = Demo_language.Server.create () in
  ignore (initialize server);
  ignore
    (open_document server ~version:1
       "id(A : Type, x : A) : A;\nid(A, x) = x;\nid(Type, Type)");
  let contents = hover server ~id:2 ~line:2 ~character:1 |> hover_contents in
  Alcotest.(check bool)
    "final body variable" true
    (contains contents "variable");
  Alcotest.(check bool) "final body text" true (contains contents "\nid\n");
  Alcotest.(check bool)
    "no concrete constructor" false (contains contents "CT_")

let test_partial_update_refreshes_hover () =
  let server = Demo_language.Server.create () in
  ignore (initialize server);
  ignore (open_document server ~version:1 "hello");
  let textDocument = VersionedTextDocumentIdentifier.create ~uri ~version:2 in
  let change =
    `TextDocumentContentChangePartial
      (TextDocumentContentChangePartial.create ~range:(range 0 0 0 5)
         ~text:"world" ())
  in
  let params =
    DidChangeTextDocumentParams.create ~textDocument ~contentChanges:[ change ]
  in
  ignore
    (Demo_language.Server.handle server
       (notification (Lsp.Client_notification.TextDocumentDidChange params)));
  let contents = hover server ~id:3 ~line:0 ~character:1 |> hover_contents in
  Alcotest.(check bool) "updated AST hover" true (contains contents "world");
  Alcotest.(check bool) "old AST discarded" false (contains contents "hello")

let test_completion () =
  let server = Demo_language.Server.create () in
  ignore (initialize server);
  ignore (open_document server ~version:1 "");
  let textDocument = TextDocumentIdentifier.create ~uri in
  let params =
    CompletionParams.create ~textDocument ~position:(position 0 0) ()
  in
  let completions =
    Demo_language.Server.handle server
      (request 2 (Lsp.Client_request.TextDocumentCompletion params))
    |> fst |> response_json |> Yojson.Safe.Util.to_list
  in
  Alcotest.(check int) "four syntax snippets" 4 (List.length completions);
  Alcotest.(check string)
    "hole completion" "_"
    (List.hd completions
    |> Yojson.Safe.Util.member "label"
    |> Yojson.Safe.Util.to_string)

let () =
  Alcotest.run "demo language server"
    [
      ( "demo",
        [
          Alcotest.test_case "lexer" `Quick test_lexer;
          Alcotest.test_case "parser diagnostics" `Quick test_parser_diagnostic;
          Alcotest.test_case "typer diagnostics" `Quick test_typer_diagnostic;
          Alcotest.test_case "Boolean example reaches vendored typer" `Quick
            test_bool_reaches_vendored_typer;
          Alcotest.test_case "Type example type checks" `Quick
            test_type_example_type_checks;
          Alcotest.test_case "documented core type checks" `Quick
            test_documented_core_type_checks;
          Alcotest.test_case "lowered syntax printer" `Quick test_syntax_printer;
          Alcotest.test_case "printer parentheses" `Quick
            test_printer_inserts_parentheses;
          Alcotest.test_case "AST hover" `Quick test_ast_hover;
          Alcotest.test_case "documented syntax hover" `Quick
            test_documented_syntax_hover;
          Alcotest.test_case "sequence body hover" `Quick
            test_sequence_body_hover;
          Alcotest.test_case "partial update hover" `Quick
            test_partial_update_refreshes_hover;
          Alcotest.test_case "completion" `Quick test_completion;
        ] );
    ]
