open Lsp_error

let on_request (type response) context _channel
    (request : response Lsp.Client_request.t) : response =
  let open Lsp_request in
  let open Lsp.Client_request in
  (* TODO: use channel? *)
  match request with
  | Initialize params -> Server_life_cycle.initialize context ~params
  | TextDocumentHover params ->
      (* TODO: this should never crash the lsp *)
      Hover.hover context ~params
  | _request ->
      (* TODO: print which requests are not supported *)
      fail Error_unsupported_request

let on_request_error _context _channel error =
  let open Jsonrpc.Response.Error in
  (* TODO: maybe error should show to user? *)
  (* TODO: better errors *)
  let message =
    match error with
    | Lsp_error { error } -> Lsp_error.show error
    | error -> Printexc.to_string error
  in
  Jsonrpc.Response.Error.make ~code:Code.InternalError ~message ()

let on_notification context _channel notification =
  let open Lsp_notification in
  let open Lsp.Client_notification in
  (* TODO: use channel? *)
  match notification with
  | Initialized -> Server_life_cycle.initialized context
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
      fail Error_notification_before_initialize
  | Running ->
      let notifications = on_notification context channel notification in
      List.iter
        (fun notification -> Lsp_channel.notify channel notification)
        notifications

let on_notification_error _context channel error =
  let open Lsp.Types in
  let open Lsp.Server_notification in
  let message =
    match error with
    | Lsp_error { error } -> Lsp_error.show error
    | error -> Printexc.to_string error
  in
  (* TODO: maybe error should show to user? *)
  let message = LogMessageParams.create ~type_:Error ~message in
  Lsp_channel.notify channel @@ LogMessage message

let main () =
  Eio_main.run @@ fun env ->
  let context = Lsp_context.create () in
  let on_request channel request =
    try Ok (on_request context channel request)
    with error -> Error (on_request_error context channel error)
  in
  let on_notification channel notification =
    try on_notification context channel notification
    with error -> on_notification_error context channel error
  in
  Lsp_channel.listen ~input:env#stdin ~output:env#stdout
    ~on_request:{ f = on_request } ~on_notification

let () = main ()
