open Lsp.Types

let uri = DocumentUri.of_path "/tmp/example.mock"
let language_id = LanguageKind.Other "mock"
let position line character = Position.create ~line ~character

let range start_line start_character end_line end_character =
  Range.create
    ~start:(position start_line start_character)
    ~end_:(position end_line end_character)

let partial ?range_length range text =
  `TextDocumentContentChangePartial
    (TextDocumentContentChangePartial.create ~range ?rangeLength:range_length
       ~text ())

let whole text =
  `TextDocumentContentChangeWholeDocument
    (TextDocumentContentChangeWholeDocument.create ~text)

let unwrap_edit = function
  | Ok value -> value
  | Error error ->
      Alcotest.fail (Language_server.Text_document.string_of_edit_error error)

let make_document text =
  Language_server.Text_document.create ~uri ~language_id ~version:1 ~text

let test_sequential_partial_changes () =
  let document = make_document "one two" in
  let changes =
    [
      partial (range 0 0 0 3) "three";
      partial ~range_length:3 (range 0 6 0 9) "four";
    ]
  in
  let document, applied =
    Language_server.Text_document.apply_changes document ~version:2 changes
    |> unwrap_edit
  in
  Alcotest.(check string)
    "changes are sequential" "three four"
    (Language_server.Text_document.text document);
  Alcotest.(check int)
    "new version" 2
    (Language_server.Text_document.version document);
  match applied with
  | [ first; second ] ->
      Alcotest.(check int)
        "first start" 0
        (Language_server.Text_document.Change.start_offset first);
      Alcotest.(check string)
        "first replaced text" "one"
        (Language_server.Text_document.Change.replaced_text first);
      Alcotest.(check int)
        "second offset uses updated text" 6
        (Language_server.Text_document.Change.start_offset second);
      Alcotest.(check (option int))
        "range length retained" (Some 3)
        (Language_server.Text_document.Change.range_length second)
  | _ -> Alcotest.fail "expected two normalized changes"

let test_utf16_positions () =
  let document = make_document "a😀b\r\nx" in
  let offset =
    Language_server.Text_document.offset_of_position document (position 0 3)
    |> unwrap_edit
  in
  Alcotest.(check int) "UTF-16 position after emoji" 5 offset;
  let converted =
    Language_server.Text_document.position_of_offset document 5 |> unwrap_edit
  in
  Alcotest.(check int) "converted line" 0 converted.line;
  Alcotest.(check int) "converted UTF-16 character" 3 converted.character;
  let clamped =
    Language_server.Text_document.offset_of_position document (position 0 99)
    |> unwrap_edit
  in
  Alcotest.(check int) "position past line end is clamped" 6 clamped;
  let document, applied =
    Language_server.Text_document.apply_changes document ~version:2
      [ partial (range 0 1 0 3) "X" ]
    |> unwrap_edit
  in
  Alcotest.(check string)
    "replace astral code point" "aXb\r\nx"
    (Language_server.Text_document.text document);
  (match applied with
  | [ change ] ->
      Alcotest.(check int)
        "emoji starts at byte one" 1
        (Language_server.Text_document.Change.start_offset change);
      Alcotest.(check int)
        "emoji occupies four UTF-8 bytes" 5
        (Language_server.Text_document.Change.end_offset change)
  | _ -> Alcotest.fail "expected one normalized change");
  match
    Language_server.Text_document.offset_of_position (make_document "😀")
      (position 0 1)
  with
  | Error (Invalid_position _) -> ()
  | Ok _ | Error _ -> Alcotest.fail "a position may not split a surrogate pair"

let test_full_change () =
  let document = make_document "old" in
  let document, applied =
    Language_server.Text_document.apply_changes document ~version:4
      [ whole "new" ]
    |> unwrap_edit
  in
  Alcotest.(check string)
    "full text" "new"
    (Language_server.Text_document.text document);
  match applied with
  | [ change ] ->
      Alcotest.(check bool)
        "full change has no range" true
        (Language_server.Text_document.Change.range change = None);
      Alcotest.(check string)
        "old text retained" "old"
        (Language_server.Text_document.Change.replaced_text change)
  | _ -> Alcotest.fail "expected one full change"

module Mock_language = struct
  type state = { mutable notifications : int }
  type analysis = string

  include Language_server.Language_defaults

  let update_calls = ref []

  let configuration =
    Language_server.Configuration.create ~name:"test language server"
      ~version:"1.0" ~language_ids:[ language_id ] ()

  let initialize _params = { notifications = 0 }
  let analyze _state document = Language_server.Text_document.text document

  let update =
    Some
      (fun _state ~previous:_ ~old_document:_ ~new_document ~changes ->
        update_calls := changes;
        Language_server.Text_document.text new_document)

  let occurrences line needle =
    let line_length = String.length line in
    let needle_length = String.length needle in
    let rec loop offset found =
      if offset + needle_length > line_length then List.rev found
      else if String.sub line offset needle_length = needle then
        loop (offset + needle_length) (offset :: found)
      else loop (offset + 1) found
    in
    loop 0 []

  let diagnostics _state analysis _document =
    String.split_on_char '\n' analysis
    |> List.mapi (fun line text ->
        occurrences text "ERROR"
        |> List.map (fun character ->
            Diagnostic.create
              ~range:(range line character line (character + 5))
              ~severity:DiagnosticSeverity.Error ~source:"mock"
              ~message:(`String "mock error") ()))
    |> List.flatten

  let identifier_at text offset =
    let is_identifier = function
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
      | _ -> false
    in
    if
      offset < 0
      || offset >= String.length text
      || not (is_identifier text.[offset])
    then None
    else
      let start = ref offset in
      let end_ = ref (offset + 1) in
      while !start > 0 && is_identifier text.[!start - 1] do
        decr start
      done;
      while !end_ < String.length text && is_identifier text.[!end_] do
        incr end_
      done;
      Some (String.sub text !start (!end_ - !start))

  let hover _state analysis document (params : HoverParams.t) =
    match
      Language_server.Text_document.offset_of_position document params.position
    with
    | Error _ -> None
    | Ok offset ->
        Option.map
          (fun identifier ->
            let contents =
              MarkupContent.create ~kind:MarkupKind.Markdown
                ~value:("Mock symbol: `" ^ identifier ^ "`")
            in
            Hover.create ~contents:(`MarkupContent contents) ())
          (identifier_at analysis offset)

  let complete _state _analysis _document _params =
    Some
      (`List
         [
           CompletionItem.create ~kind:CompletionItemKind.Keyword ~label:"mock"
             ();
         ])

  let definition _state _analysis document _params =
    let location =
      Location.create
        ~uri:(Language_server.Text_document.uri document)
        ~range:(range 0 0 0 1)
    in
    Some (`Location [ location ])

  let references _state _analysis document _params =
    Some
      [
        Location.create
          ~uri:(Language_server.Text_document.uri document)
          ~range:(range 0 0 0 1);
      ]

  let document_symbols _state _analysis _document _params =
    let symbol =
      DocumentSymbol.create ~name:"mock" ~kind:SymbolKind.Variable
        ~range:(range 0 0 0 4) ~selectionRange:(range 0 0 0 4) ()
    in
    Some (`DocumentSymbol [ symbol ])

  let formatting _state analysis document _params =
    Some
      [
        TextEdit.create
          ~range:(Language_server.Text_document.full_range document)
          ~newText:(String.trim analysis ^ "\n");
      ]

  let custom_request _state ~method_ ~params:_ =
    if method_ = "mock/ping" then Some (Ok (`String "pong")) else None

  let features : (state, analysis) Language_server.Features.t =
    {
      Language_server.Features.none with
      hover = Some hover;
      completion = Some { trigger_characters = [ "." ]; complete };
      definition = Some definition;
      references = Some references;
      document_symbols = Some document_symbols;
      formatting = Some formatting;
      custom_request = Some custom_request;
    }
end

module Server = Language_server.Make (Mock_language)

module Full_analysis_language = struct
  type state = unit
  type analysis = string

  include Language_server.Language_defaults

  let analysis_calls = ref 0

  let configuration =
    Language_server.Configuration.create ~name:"full analysis server"
      ~language_ids:[ language_id ] ()

  let initialize _params = ()

  let analyze () document =
    incr analysis_calls;
    Language_server.Text_document.text document

  let diagnostics () _analysis _document = []
end

module Full_analysis_server = Language_server.Make (Full_analysis_language)

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
  let packets, action =
    Server.handle server (request 1 (Lsp.Client_request.Initialize params))
  in
  Alcotest.(check bool) "continue" true (action = Server.Continue);
  response_json packets

let open_document server ~version ~text =
  let textDocument =
    TextDocumentItem.create ~uri ~languageId:language_id ~version ~text
  in
  Server.handle server
    (notification
       (Lsp.Client_notification.TextDocumentDidOpen
          (DidOpenTextDocumentParams.create ~textDocument)))

let change_document server ~version contentChanges =
  let textDocument = VersionedTextDocumentIdentifier.create ~uri ~version in
  let params =
    DidChangeTextDocumentParams.create ~textDocument ~contentChanges
  in
  Server.handle server
    (notification (Lsp.Client_notification.TextDocumentDidChange params))

let test_capabilities_and_lifecycle () =
  let server = Server.create () in
  let initialize_json = initialize server in
  let capabilities = Yojson.Safe.Util.member "capabilities" initialize_json in
  let synchronization =
    Yojson.Safe.Util.member "textDocumentSync" capabilities
  in
  Alcotest.(check int)
    "incremental synchronization" 2
    (Yojson.Safe.Util.member "change" synchronization |> Yojson.Safe.Util.to_int);
  Alcotest.(check bool)
    "open and close synchronization" true
    (Yojson.Safe.Util.member "openClose" synchronization
    |> Yojson.Safe.Util.to_bool);
  Alcotest.(check bool)
    "save includes text" true
    (Yojson.Safe.Util.member "save" synchronization
    |> Yojson.Safe.Util.member "includeText"
    |> Yojson.Safe.Util.to_bool);
  Alcotest.(check bool)
    "hover advertised" true
    (Yojson.Safe.Util.member "hoverProvider" capabilities
    |> Yojson.Safe.Util.to_bool);
  Alcotest.(check string)
    "completion triggers advertised" "."
    (Yojson.Safe.Util.member "completionProvider" capabilities
    |> Yojson.Safe.Util.member "triggerCharacters"
    |> Yojson.Safe.Util.to_list |> List.hd |> Yojson.Safe.Util.to_string);
  let packets, _ =
    Server.handle server (request 2 Lsp.Client_request.Shutdown)
  in
  ignore (response_json packets);
  let _, action =
    Server.handle server (notification Lsp.Client_notification.Exit)
  in
  Alcotest.(check bool) "clean exit" true (action = Server.Exit 0)

let test_incremental_server_update () =
  let server = Server.create () in
  ignore (initialize server);
  ignore (open_document server ~version:1 ~text:"one two ERROR");
  let packets, _ =
    change_document server ~version:2
      [ partial (range 0 0 0 3) "three"; partial (range 0 10 0 15) "ok" ]
  in
  (match packets with
  | [ Jsonrpc.Packet.Notification notification ] ->
      let diagnostics =
        Jsonrpc.Notification.yojson_of_t notification
        |> Yojson.Safe.Util.member "params"
        |> Yojson.Safe.Util.member "diagnostics"
        |> Yojson.Safe.Util.to_list
      in
      Alcotest.(check int) "diagnostics refreshed" 0 (List.length diagnostics)
  | _ -> Alcotest.fail "expected diagnostics notification");
  let document =
    match Server.find_document server uri with
    | Some document -> document
    | None -> Alcotest.fail "document should be open"
  in
  Alcotest.(check string)
    "server text is updated" "three two ok"
    (Language_server.Text_document.text document);
  (match !Mock_language.update_calls with
  | [ first; second ] ->
      Alcotest.(check string)
        "first incremental replacement" "one"
        (Language_server.Text_document.Change.replaced_text first);
      Alcotest.(check int)
        "second incremental offset" 10
        (Language_server.Text_document.Change.start_offset second)
  | _ -> Alcotest.fail "incremental callback should receive both edits");
  Mock_language.update_calls := [];
  ignore
    (change_document server ~version:3 [ partial (range 99 0 99 1) "invalid" ]);
  let document = Option.get (Server.find_document server uri) in
  Alcotest.(check string)
    "invalid edit is atomic" "three two ok"
    (Language_server.Text_document.text document);
  Alcotest.(check int)
    "invalid edit does not advance version" 2
    (Language_server.Text_document.version document);
  Alcotest.(check int)
    "invalid edit does not invoke language update" 0
    (List.length !Mock_language.update_calls)

let test_full_analysis_fallback () =
  Full_analysis_language.analysis_calls := 0;
  let server = Full_analysis_server.create () in
  let capabilities = ClientCapabilities.create () in
  let initialize_params = InitializeParams.create ~capabilities () in
  ignore
    (Full_analysis_server.handle server
       (request 1 (Lsp.Client_request.Initialize initialize_params)));
  let item =
    TextDocumentItem.create ~uri ~languageId:language_id ~version:1
      ~text:"old text"
  in
  ignore
    (Full_analysis_server.handle server
       (notification
          (Lsp.Client_notification.TextDocumentDidOpen
             (DidOpenTextDocumentParams.create ~textDocument:item))));
  let identifier = VersionedTextDocumentIdentifier.create ~uri ~version:2 in
  let params =
    DidChangeTextDocumentParams.create ~textDocument:identifier
      ~contentChanges:[ partial (range 0 0 0 3) "new" ]
  in
  ignore
    (Full_analysis_server.handle server
       (notification (Lsp.Client_notification.TextDocumentDidChange params)));
  Alcotest.(check int)
    "open and change both analyze" 2
    !Full_analysis_language.analysis_calls;
  let document = Option.get (Full_analysis_server.find_document server uri) in
  Alcotest.(check string)
    "fallback sees partially updated text" "new text"
    (Language_server.Text_document.text document)

let text_document_identifier () = TextDocumentIdentifier.create ~uri

let test_hover () =
  let server = Server.create () in
  ignore (initialize server);
  ignore (open_document server ~version:1 ~text:"hello");
  let textDocument = text_document_identifier () in
  let hover_params =
    HoverParams.create ~textDocument ~position:(position 0 1) ()
  in
  let hover =
    Server.handle server
      (request 2 (Lsp.Client_request.TextDocumentHover hover_params))
    |> fst |> response_json
  in
  Alcotest.(check string)
    "hover" "Mock symbol: `hello`"
    (Yojson.Safe.Util.member "contents" hover
    |> Yojson.Safe.Util.member "value"
    |> Yojson.Safe.Util.to_string);
  let missing_params =
    HoverParams.create ~textDocument ~position:(position 0 5) ()
  in
  let missing =
    Server.handle server
      (request 3 (Lsp.Client_request.TextDocumentHover missing_params))
    |> fst |> response_json
  in
  Alcotest.(check bool) "no symbol at position" true (missing = `Null)

let test_common_feature_requests () =
  let server = Server.create () in
  ignore (initialize server);
  ignore (open_document server ~version:1 ~text:"hello");
  let textDocument = text_document_identifier () in
  let completion_params =
    CompletionParams.create ~textDocument ~position:(position 0 5) ()
  in
  let completion =
    Server.handle server
      (request 3 (Lsp.Client_request.TextDocumentCompletion completion_params))
    |> fst |> response_json
  in
  Alcotest.(check string)
    "completion" "mock"
    (Yojson.Safe.Util.to_list completion
    |> List.hd
    |> Yojson.Safe.Util.member "label"
    |> Yojson.Safe.Util.to_string);
  let definition_params =
    DefinitionParams.create ~textDocument ~position:(position 0 1) ()
  in
  let definition =
    Server.handle server
      (request 4 (Lsp.Client_request.TextDocumentDefinition definition_params))
    |> fst |> response_json
  in
  Alcotest.(check int)
    "definition" 1
    (Yojson.Safe.Util.to_list definition |> List.length);
  let reference_context = ReferenceContext.create ~includeDeclaration:true in
  let reference_params =
    ReferenceParams.create ~context:reference_context ~textDocument
      ~position:(position 0 1) ()
  in
  let references =
    Server.handle server
      (request 5 (Lsp.Client_request.TextDocumentReferences reference_params))
    |> fst |> response_json
  in
  Alcotest.(check int)
    "references" 1
    (Yojson.Safe.Util.to_list references |> List.length);
  let symbol_params = DocumentSymbolParams.create ~textDocument () in
  let symbols =
    Server.handle server
      (request 6 (Lsp.Client_request.DocumentSymbol symbol_params))
    |> fst |> response_json
  in
  Alcotest.(check string)
    "document symbols" "mock"
    (Yojson.Safe.Util.to_list symbols
    |> List.hd
    |> Yojson.Safe.Util.member "name"
    |> Yojson.Safe.Util.to_string);
  let options = FormattingOptions.create ~insertSpaces:true ~tabSize:2 () in
  let formatting_params =
    DocumentFormattingParams.create ~options ~textDocument ()
  in
  let edits =
    Server.handle server
      (request 7 (Lsp.Client_request.TextDocumentFormatting formatting_params))
    |> fst |> response_json
  in
  Alcotest.(check string)
    "formatting" "hello\n"
    (Yojson.Safe.Util.to_list edits
    |> List.hd
    |> Yojson.Safe.Util.member "newText"
    |> Yojson.Safe.Util.to_string);
  let raw_request =
    Jsonrpc.Request.create ~id:(`Int 8) ~method_:"mock/ping" ()
  in
  let custom =
    Server.handle server (Jsonrpc.Packet.Request raw_request)
    |> fst |> response_json
  in
  Alcotest.(check string)
    "custom request" "pong"
    (Yojson.Safe.Util.to_string custom)

let frame packet =
  let payload = Jsonrpc.Packet.yojson_of_t packet |> Yojson.Safe.to_string in
  Printf.sprintf "Content-Length: %d\r\n\r\n%s" (String.length payload) payload

let read_packets input =
  let prefix = "content-length:" in
  let rec read_header content_length =
    match input_line input with
    | exception End_of_file -> None
    | line ->
        let line = String.trim line in
        if line = "" then Some content_length
        else
          let lowercase = String.lowercase_ascii line in
          let content_length =
            if String.starts_with ~prefix lowercase then
              String.sub line (String.length prefix)
                (String.length line - String.length prefix)
              |> String.trim |> int_of_string
            else content_length
          in
          read_header content_length
  in
  let rec loop packets =
    match read_header (-1) with
    | None -> List.rev packets
    | Some length when length >= 0 ->
        let json =
          really_input_string input length |> Yojson.Safe.from_string
        in
        loop (Jsonrpc.Packet.t_of_yojson json :: packets)
    | Some _ -> Alcotest.fail "missing Content-Length header"
  in
  loop []

let test_stdio_transport () =
  let input_path, input_writer =
    Filename.open_temp_file "language-server" ".in"
  in
  let output_path, output = Filename.open_temp_file "language-server" ".out" in
  let input = ref None in
  Fun.protect
    ~finally:(fun () ->
      Option.iter close_in_noerr !input;
      close_out_noerr input_writer;
      close_out_noerr output;
      Sys.remove input_path;
      Sys.remove output_path)
    (fun () ->
      let capabilities = ClientCapabilities.create () in
      let params = InitializeParams.create ~capabilities () in
      let packets =
        [
          request 1 (Lsp.Client_request.Initialize params);
          request 2 Lsp.Client_request.Shutdown;
          notification Lsp.Client_notification.Exit;
        ]
      in
      List.iter
        (fun packet -> output_string input_writer (frame packet))
        packets;
      close_out input_writer;
      let input_channel = open_in_bin input_path in
      input := Some input_channel;
      let code = Server.run ~input:input_channel ~output () in
      close_out output;
      Alcotest.(check int) "clean process exit" 0 code;
      let output = open_in_bin output_path in
      let packets =
        Fun.protect
          ~finally:(fun () -> close_in_noerr output)
          (fun () -> read_packets output)
      in
      Alcotest.(check int)
        "initialize and shutdown responses" 2 (List.length packets))

let () =
  Alcotest.run "language server"
    [
      ( "text document",
        [
          Alcotest.test_case "sequential partial changes" `Quick
            test_sequential_partial_changes;
          Alcotest.test_case "UTF-16 positions" `Quick test_utf16_positions;
          Alcotest.test_case "full change" `Quick test_full_change;
        ] );
      ( "server",
        [
          Alcotest.test_case "capabilities and lifecycle" `Quick
            test_capabilities_and_lifecycle;
          Alcotest.test_case "incremental update" `Quick
            test_incremental_server_update;
          Alcotest.test_case "full analysis fallback" `Quick
            test_full_analysis_fallback;
          Alcotest.test_case "hover" `Quick test_hover;
          Alcotest.test_case "common feature requests" `Quick
            test_common_feature_requests;
          Alcotest.test_case "stdio transport" `Quick test_stdio_transport;
        ] );
    ]
