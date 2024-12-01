open Lsp.Types
open Lsp_context
open Lsp_error

module Server_life_cycle = struct
  (* TODO: do something here?*)
  let initialized _context = []
end

module Text_document_sync = struct
  (* TODO: put this somewhere else *)
  let position_of_pos pos =
    let Lexing.{ pos_fname = _; pos_lnum; pos_bol; pos_cnum } = pos in
    let line = pos_lnum - 1 in
    let character = pos_cnum - pos_bol in
    Position.create ~line ~character

  let range_of_loc loc =
    let open Ocaml_common in
    let Location.{ loc_start; loc_end; loc_ghost = _ } = loc in
    match Location.is_none loc with
    | true ->
        (* TODO: better than this? *)
        let start = Position.create ~line:0 ~character:0 in
        let end_ = Position.create ~line:0 ~character:0 in
        Range.create ~start ~end_
    | false ->
        (* TODO: what if None location? *)
        let start = position_of_pos loc_start in
        let end_ = position_of_pos loc_end in
        Range.create ~start ~end_

  let publish_diagnostics ~uri ~version document =
    let open Lsp_text_document in
    let diagnostics = diagnostics document in
    let diagnostics =
      List.map
        (fun { loc; message } ->
          let range = range_of_loc loc in
          let message = `String message in
          Diagnostic.create ~range ~message ())
        diagnostics
    in
    [
      Lsp.Server_notification.PublishDiagnostics
        (PublishDiagnosticsParams.create ~uri ~diagnostics ~version ());
    ]

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
    open_text_document context uri document;
    publish_diagnostics ~uri ~version document

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
    let document =
      change_text_document context uri @@ fun document ->
      (* TODO: async typing here *)
      Lsp_text_document.with_change ~version ~text document
    in
    publish_diagnostics ~uri ~version document

  let did_close context ~params =
    let DidCloseTextDocumentParams.{ textDocument = { uri } } = params in
    let () = close_text_document context uri in
    []

  (* TODO: save and rename *)
end
