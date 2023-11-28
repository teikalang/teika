open Lsp.Types
open Lsp_error
module Document_uri_map = Map.Make (DocumentUri)

(* TODO: capabilities *)
(* TODO: initialized *)
type status = Handshake | Running

type context = {
  mutable status : status;
  mutable text_documents : Lsp_text_document.t Document_uri_map.t;
}

type t = context

let create () = { status = Handshake; text_documents = Document_uri_map.empty }
let status context = context.status

let initialize context =
  match context.status with
  | Handshake -> context.status <- Running
  | Running -> fail Error_invalid_status_during_initialize

let update_text_documents context f =
  let text_documents = context.text_documents in
  let text_documents = f text_documents in
  context.text_documents <- text_documents

let open_text_document context uri text_document =
  update_text_documents context @@ fun text_documents ->
  (match Document_uri_map.mem uri text_documents with
  | true -> fail Error_text_document_already_in_context
  | false -> ());
  Document_uri_map.add uri text_document text_documents

let change_text_document context uri cb =
  update_text_documents context @@ fun text_documents ->
  let text_document =
    match Document_uri_map.find_opt uri text_documents with
    | Some text_document -> text_document
    | None -> fail Error_text_document_not_in_context
  in
  let text_document = cb text_document in
  (* TODO: only accept if version is newer or equal *)
  Document_uri_map.add uri text_document text_documents

let close_text_document context uri =
  update_text_documents context @@ fun text_documents ->
  (match Document_uri_map.mem uri text_documents with
  | true -> ()
  | false -> fail Error_text_document_not_in_context);
  Document_uri_map.remove uri text_documents
