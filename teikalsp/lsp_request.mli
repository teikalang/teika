open Lsp.Types

module Server_life_cycle : sig
  val initialize :
    Lsp_context.t -> params:InitializeParams.t -> InitializeResult.t
end
