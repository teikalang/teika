open Lsp.Types
open Lsp_context
open Lsp_error

module Text_document_sync = struct
  let did_open context ~params =
    let DidOpenTextDocumentParams.{ textDocument = text_document } = params in
    let TextDocumentItem.{ uri; languageId = language_id; version; text } =
      text_document
    in
    let document =
      match language_id with
      | "teika" -> Lsp_text_document.teika ~version ~text
      | language_id -> fail (Error_unknown_language_id { language_id })
    in
    (* TODO: async typing here *)
    open_text_document context uri document

  let did_change context ~params =
    (* TODO: currently only full content changes are supported
        partial content changes could be supported *)
    let DidChangeTextDocumentParams.
          { textDocument = { uri; version }; contentChanges = content_changes }
        =
      params
    in
    let content_change =
      match content_changes with
      | [ content_change ] -> content_change
      | content_changes ->
          fail (Error_multiple_content_changes { content_changes })
    in
    let TextDocumentContentChangeEvent.{ range; rangeLength; text } =
      content_change
    in
    (match (range, rangeLength) with
    | None, None -> ()
    | Some _, Some _ -> fail (Error_partial_content_change { content_change })
    | Some _, None | None, Some _ ->
        fail (Error_invalid_content_change { content_change }));
    change_text_document context uri @@ fun document ->
    (* TODO: async typing here *)
    Lsp_text_document.with_change ~version ~text document

  let did_close context ~params =
    let DidCloseTextDocumentParams.{ textDocument = { uri } } = params in
    close_text_document context uri

  (* TODO: save and rename *)
end
