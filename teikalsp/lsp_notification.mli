open Lsp.Types

module Server_life_cycle : sig
  val initialized : Lsp_context.t -> Lsp.Server_notification.t list
end

module Text_document_sync : sig
  val did_open :
    Lsp_context.t ->
    params:DidOpenTextDocumentParams.t ->
    Lsp.Server_notification.t list

  val did_change :
    Lsp_context.t ->
    params:DidChangeTextDocumentParams.t ->
    Lsp.Server_notification.t list

  val did_close :
    Lsp_context.t ->
    params:DidCloseTextDocumentParams.t ->
    Lsp.Server_notification.t list
end
