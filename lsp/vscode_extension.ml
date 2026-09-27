type extension_context
type workspace
type workspace_folder
type uri
type window
type process_options
type server_options
type document_filter
type client_options
type language_client

external workspace : workspace = "workspace" [@@mel.module "vscode"]
external window : window = "window" [@@mel.module "vscode"]

external workspace_folders : workspace -> workspace_folder array Js.nullable
  = "workspaceFolders"
[@@mel.get]

external folder_uri : workspace_folder -> uri = "uri" [@@mel.get]
external uri_path : uri -> string = "fsPath" [@@mel.get]
external array_get : 'a array -> int -> 'a = "" [@@mel.get_index]

external show_error_message : window -> string -> unit Js.Promise.t
  = "showErrorMessage"
[@@mel.send]

external process_options : cwd:string -> unit -> process_options = ""
[@@mel.obj]

external server_options :
  command:string ->
  args:string array ->
  options:process_options ->
  unit ->
  server_options = ""
[@@mel.obj]

external document_filter :
  scheme:string -> language:string -> unit -> document_filter = ""
[@@mel.obj]

external client_options :
  documentSelector:document_filter array -> unit -> client_options = ""
[@@mel.obj]

external language_client :
  string -> string -> server_options -> client_options -> language_client
  = "LanguageClient"
[@@mel.module "vscode-languageclient/node"] [@@mel.new]

external start : language_client -> unit Js.Promise.t = "start" [@@mel.send]
external stop : language_client -> unit Js.Promise.t = "stop" [@@mel.send]

let active_client = ref None

let workspace_path () =
  match Js.Nullable.toOption (workspace_folders workspace) with
  | Some folders when Array.length folders > 0 ->
      Some (uri_path (folder_uri (array_get folders 0)))
  | Some _ | None -> None

let make_client root =
  let command = root ^ "/lsp/_build/default/lsp/demo_lsp.exe" in
  let server =
    server_options ~command ~args:[||]
      ~options:(process_options ~cwd:root ())
      ()
  in
  let selector = document_filter ~scheme:"file" ~language:"teika" () in
  let client = client_options ~documentSelector:[| selector |] () in
  language_client "teika" "Teika language server" server client

let activate (_context : extension_context) =
  match workspace_path () with
  | None ->
      Js.Promise.then_
        (fun _ -> Js.Promise.resolve ())
        (show_error_message window
           "Teika needs an open workspace containing the lsp/ directory.")
  | Some root ->
      let client = make_client root in
      active_client := Some client;
      start client

let deactivate () =
  match !active_client with
  | None -> Js.Promise.resolve ()
  | Some client -> stop client
