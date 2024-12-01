open Lsp.Types

type status = private Handshake | Running
type context
type t = context

(* TODO: rollback? Requests and notifications should probably be atomic *)
val create : unit -> context
val status : context -> status
val initialize : context -> unit

(* documents *)
val find_document : context -> DocumentUri.t -> Lsp_text_document.t
val open_text_document : context -> DocumentUri.t -> Lsp_text_document.t -> unit

val change_text_document :
  context ->
  DocumentUri.t ->
  (Lsp_text_document.t -> Lsp_text_document.t) ->
  Lsp_text_document.t

val close_text_document : context -> DocumentUri.t -> unit
