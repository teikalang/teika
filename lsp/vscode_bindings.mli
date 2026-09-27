(** Minimal typed bindings used by the VS Code extension-host probe. *)

type uri
type document
type text_editor
type position
type hover
type completion_list
type diagnostic

val workspace_path : unit -> string option
val open_document : string -> document Js.Promise.t
val show_document : document -> text_editor Js.Promise.t
val document_language_id : document -> string
val document_uri : document -> uri
val position : line:int -> character:int -> position
val request_hovers : uri -> position -> hover array Js.Promise.t
val hovers_contain : hover array -> string -> bool
val request_completions : uri -> position -> completion_list Js.Promise.t
val completion_has_label : completion_list -> string -> bool
val diagnostics : uri -> diagnostic array
val sleep : int -> bool Js.Promise.t
val log : string -> unit
