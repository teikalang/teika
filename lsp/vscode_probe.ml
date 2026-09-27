module Vscode = Vscode_bindings

module Promise = struct
  let ( let* ) promise continuation = Js.Promise.then_ continuation promise
  let return = Js.Promise.resolve
end

open Promise

let fail message =
  let message = "VS Code integration: " ^ message in
  Vscode.log message;
  failwith message

module Await = struct
  let rec hover uri position = function
    | 0 -> fail "hover provider returned no results"
    | attempts ->
        let* hovers = Vscode.request_hovers uri position in
        if Array.length hovers > 0 then return hovers
        else
          let* _ = Vscode.sleep 250 in
          hover uri position (attempts - 1)

  let rec completion uri position label = function
    | 0 -> fail ("the '" ^ label ^ "' completion item was not returned")
    | attempts ->
        let* response = Vscode.request_completions uri position in
        if Vscode.completion_has_label response label then return ()
        else
          let* _ = Vscode.sleep 250 in
          completion uri position label (attempts - 1)

  let rec diagnostics uri = function
    | 0 -> fail "the demo diagnostic was not published"
    | attempts ->
        if Array.length (Vscode.diagnostics uri) > 0 then return ()
        else
          let* _ = Vscode.sleep 250 in
          diagnostics uri (attempts - 1)
end

let check_hover hovers source =
  if not (Vscode.hovers_contain hovers ("\n" ^ source ^ "\n")) then
    fail ("hover did not contain printed Teika source " ^ source);
  if Vscode.hovers_contain hovers "CT_" then
    fail "hover exposed a derived concrete-tree constructor"

let workspace_path () =
  match Vscode.workspace_path () with
  | Some path -> path
  | None -> fail "the test needs an open workspace folder"

let check_language document =
  if not (String.equal (Vscode.document_language_id document) "teika") then
    fail "a .tei document was not registered as the 'teika' language"

let open_example root name =
  let* document = Vscode.open_document (root ^ "/examples/" ^ name) in
  check_language document;
  let* _editor = Vscode.show_document document in
  return document

let check_well_typed_example root =
  let* document = open_example root "type.tei" in
  let uri = Vscode.document_uri document in
  let position = Vscode.position ~line:1 ~character:1 in
  let* hovers = Await.hover uri position 40 in
  check_hover hovers "Type";
  let* _ = Vscode.sleep 250 in
  if Array.length (Vscode.diagnostics uri) > 0 then
    fail "examples/type.tei unexpectedly produced a diagnostic";
  return ()

let check_core_example root =
  let* document = open_example root "core.tei" in
  let uri = Vscode.document_uri document in
  let position = Vscode.position ~line:3 ~character:1 in
  let* hovers = Await.hover uri position 40 in
  check_hover hovers "id";
  let* _ = Vscode.sleep 250 in
  if Array.length (Vscode.diagnostics uri) > 0 then
    fail "examples/core.tei unexpectedly produced a diagnostic";
  return ()

let check_diagnostic_example root =
  let* document = open_example root "bool.tei" in
  let uri = Vscode.document_uri document in
  let hover_position = Vscode.position ~line:3 ~character:1 in
  let completion_position = Vscode.position ~line:3 ~character:4 in
  let* hovers = Await.hover uri hover_position 40 in
  check_hover hovers "Bool";
  let* () = Await.completion uri completion_position "_" 40 in
  Await.diagnostics uri 40

let run () =
  let root = workspace_path () in
  let* () = check_well_typed_example root in
  let* () = check_core_example root in
  let* () = check_diagnostic_example root in
  Vscode.log
    "Teika VS Code integration: typing, hover, completion, and diagnostics \
     passed";
  return ()
