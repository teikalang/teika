type workspace
type workspace_folder
type uri
type document
type window
type text_editor
type commands
type languages
type position
type hover
type markdown_string
type completion_list
type completion_item
type diagnostic

external workspace : workspace = "workspace" [@@mel.module "vscode"]
external window : window = "window" [@@mel.module "vscode"]
external commands : commands = "commands" [@@mel.module "vscode"]
external languages : languages = "languages" [@@mel.module "vscode"]

external workspace_folders : workspace -> workspace_folder array Js.nullable
  = "workspaceFolders"
[@@mel.get]

external folder_uri : workspace_folder -> uri = "uri" [@@mel.get]
external uri_path : uri -> string = "fsPath" [@@mel.get]

external open_text_document : workspace -> string -> document Js.Promise.t
  = "openTextDocument"
[@@mel.send]

external document_language_id : document -> string = "languageId" [@@mel.get]
external document_uri : document -> uri = "uri" [@@mel.get]

external show_text_document : window -> document -> text_editor Js.Promise.t
  = "showTextDocument"
[@@mel.send]

external make_position : int -> int -> position = "Position"
[@@mel.module "vscode"] [@@mel.new]

external execute_hover_provider :
  commands -> string -> uri -> position -> hover array Js.Promise.t
  = "executeCommand"
[@@mel.send]

external hover_contents : hover -> markdown_string array = "contents"
[@@mel.get]

external markdown_value : markdown_string -> string = "value" [@@mel.get]

external execute_completion_provider :
  commands -> string -> uri -> position -> completion_list Js.Promise.t
  = "executeCommand"
[@@mel.send]

external completion_items : completion_list -> completion_item array = "items"
[@@mel.get]

external completion_label : completion_item -> string = "label" [@@mel.get]

external get_diagnostics : languages -> uri -> diagnostic array
  = "getDiagnostics"
[@@mel.send]

external set_timeout : (unit -> unit) -> int -> int = "setTimeout"
external log : string -> unit = "log" [@@mel.scope "console"]

let workspace_path () =
  match Js.Nullable.toOption (workspace_folders workspace) with
  | Some folders when Array.length folders > 0 ->
      Some (uri_path (folder_uri folders.(0)))
  | Some _ | None -> None

let open_document path = open_text_document workspace path
let show_document document = show_text_document window document
let position ~line ~character = make_position line character

let request_hovers uri position =
  execute_hover_provider commands "vscode.executeHoverProvider" uri position

let contains text substring =
  let text_length = String.length text in
  let substring_length = String.length substring in
  let rec search offset =
    if offset + substring_length > text_length then false
    else if String.sub text offset substring_length = substring then true
    else search (offset + 1)
  in
  search 0

let hovers_contain hovers substring =
  Array.exists
    (fun hover ->
      Array.exists
        (fun markdown -> contains (markdown_value markdown) substring)
        (hover_contents hover))
    hovers

let request_completions uri position =
  execute_completion_provider commands "vscode.executeCompletionItemProvider"
    uri position

let completion_has_label completion label =
  Array.exists
    (fun item -> String.equal (completion_label item) label)
    (completion_items completion)

let diagnostics uri = get_diagnostics languages uri

let sleep milliseconds =
  Js.Promise.make (fun ~resolve ~reject:_ ->
      ignore (set_timeout (fun () -> (resolve true [@u])) milliseconds))
