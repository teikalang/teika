open Lsp.Types

module Server_life_cycle : sig
  val initialize :
    Lsp_context.t -> params:InitializeParams.t -> InitializeResult.t
end

module Hover : sig
  val hover : Lsp_context.t -> params:HoverParams.t -> Hover.t option
end
