open Lsp.Types

type error =
  (* channel *)
  | Error_request_unsupported
  | Error_response_unsupported
  | Error_invalid_request of { error : string }
  | Error_invalid_notification of { error : string }
  (* server *)
  | Error_unsupported_request
  | Error_unsupported_notification
  (* context *)
  | Error_notification_before_initialize
  | Error_invalid_status_during_initialize
  | Error_text_document_already_in_context
  | Error_text_document_not_in_context
  (* notification *)
  | Error_multiple_content_changes of {
      content_changes : TextDocumentContentChangeEvent.t list; [@opaque]
    }
  | Error_partial_content_change of {
      content_change : TextDocumentContentChangeEvent.t; [@opaque]
    }
  | Error_invalid_content_change of {
      content_change : TextDocumentContentChangeEvent.t; [@opaque]
    }
  | Error_unknown_language_id of { language_id : string }

and t = error [@@deriving show]

(* TODO: what happen with errors? *)
exception Lsp_error of { error : error }

let fail error = raise (Lsp_error { error })
