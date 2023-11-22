open Lsp_error

let on_request (type response) context _channel
    (request : response Lsp.Client_request.t) : response =
  let open Lsp_request in
  let open Lsp.Client_request in
  (* TODO: use channel? *)
  match request with
  | Initialize params -> Server_life_cycle.initialize context ~params
  | _request ->
      (* TODO: print which requests are not supported *)
      fail Error_unsupported_request

let on_notification context _channel notification =
  let open Lsp_notification in
  let open Lsp.Client_notification in
  (* TODO: use channel? *)
  match notification with
  | TextDocumentDidOpen params -> Text_document_sync.did_open context ~params
  | TextDocumentDidChange params ->
      Text_document_sync.did_change context ~params
  | TextDocumentDidClose params -> Text_document_sync.did_close context ~params
  | _notification ->
      (* TODO: print which notifications are not supported *)
      fail Error_unsupported_notification

let on_notification context channel notification =
  (* TODO: notification error handling *)
  match Lsp_context.status context with
  | Handshake ->
      (* TODO: log *)
      (* TODO: server can send some notifications during handshake *)
      ()
  | Running -> on_notification context channel notification

let main () =
  Eio_main.run @@ fun env ->
  let context = Lsp_context.create () in
  let on_request channel request = on_request context channel request in
  let on_notification channel notification =
    on_notification context channel notification
  in
  Lsp_channel.listen ~input:env#stdin ~output:env#stdout
    ~on_request:{ f = on_request } ~on_notification

let () = main ()
