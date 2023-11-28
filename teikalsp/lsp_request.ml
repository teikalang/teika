open Lsp.Types
open Lsp_context
open Lsp_error

module Server_life_cycle = struct
  let initialize context ~params =
    let () =
      (* TODO: this is duplicated *)
      match status context with
      | Handshake -> ()
      | Running -> fail Error_invalid_status_during_initialize
    in
    (* TODO: use additional data *)
    let InitializeParams.
          {
            workDoneToken = _;
            processId = _;
            clientInfo = _;
            locale = _;
            rootPath = _;
            rootUri = _;
            initializationOptions = _;
            capabilities = _;
            (* TODO: definitely ignore capabilities *)
            trace = _;
            (* TODO: enable logging using tgrace*)
            workspaceFolders = _;
          } =
      params
    in
    let () = Lsp_context.initialize context in
    (* TODO: better capabilities *)
    let capabilities =
      ServerCapabilities.create ~textDocumentSync:(`TextDocumentSyncKind Full)
        ~hoverProvider:(`Bool true) ()
    in
    (* TODO: server_info *)
    InitializeResult.create ~capabilities ()
end
