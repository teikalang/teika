open Lsp.Types

module Server_life_cycle : sig
  val initialized : Lsp_context.t -> unit
end

module Text_document_sync : sig
  val did_open : Lsp_context.t -> params:DidOpenTextDocumentParams.t -> unit
  val did_change : Lsp_context.t -> params:DidChangeTextDocumentParams.t -> unit
  val did_close : Lsp_context.t -> params:DidCloseTextDocumentParams.t -> unit
end
