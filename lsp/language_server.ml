open Lsp.Types
module Text_document = Lsp_document

module Configuration = struct
  type t = {
    name : string;
    version : string option;
    language_ids : LanguageKind.t list;
    experimental_capabilities : Yojson.Safe.t option;
  }

  let create ~name ?version ~language_ids ?experimental_capabilities () =
    { name; version; language_ids; experimental_capabilities }
end

module Features = struct
  type completion_result =
    [ `CompletionList of CompletionList.t | `List of CompletionItem.t list ]
    option

  type document_symbol_result =
    [ `DocumentSymbol of DocumentSymbol.t list
    | `SymbolInformation of SymbolInformation.t list ]
    option

  type ('state, 'analysis) completion_provider = {
    trigger_characters : string list;
    complete :
      'state ->
      'analysis ->
      Text_document.t ->
      CompletionParams.t ->
      completion_result;
  }

  type ('state, 'analysis) t = {
    hover :
      ('state ->
      'analysis ->
      Text_document.t ->
      HoverParams.t ->
      Hover.t option)
      option;
    completion : ('state, 'analysis) completion_provider option;
    definition :
      ('state ->
      'analysis ->
      Text_document.t ->
      DefinitionParams.t ->
      Locations.t option)
      option;
    references :
      ('state ->
      'analysis ->
      Text_document.t ->
      ReferenceParams.t ->
      Location.t list option)
      option;
    document_symbols :
      ('state ->
      'analysis ->
      Text_document.t ->
      DocumentSymbolParams.t ->
      document_symbol_result)
      option;
    formatting :
      ('state ->
      'analysis ->
      Text_document.t ->
      DocumentFormattingParams.t ->
      TextEdit.t list option)
      option;
    custom_request :
      ('state ->
      method_:string ->
      params:Jsonrpc.Structured.t option ->
      (Yojson.Safe.t, Jsonrpc.Response.Error.t) result option)
      option;
    notification : ('state -> Lsp.Client_notification.t -> unit) option;
  }

  let none =
    {
      hover = None;
      completion = None;
      definition = None;
      references = None;
      document_symbols = None;
      formatting = None;
      custom_request = None;
      notification = None;
    }
end

module Language_defaults = struct
  let update = None
  let close_document = None
  let shutdown = None
  let features = Features.none
end

module type Language = sig
  type state
  type analysis

  val configuration : Configuration.t
  val initialize : InitializeParams.t -> state
  val analyze : state -> Text_document.t -> analysis

  val update :
    (state ->
    previous:analysis ->
    old_document:Text_document.t ->
    new_document:Text_document.t ->
    changes:Text_document.Change.t list ->
    analysis)
    option

  val diagnostics : state -> analysis -> Text_document.t -> Diagnostic.t list
  val close_document : (state -> analysis -> Text_document.t -> unit) option
  val shutdown : (state -> unit) option
  val features : (state, analysis) Features.t
end

module type Server = sig
  type t
  type status = Awaiting_initialize | Running | Shutdown_requested
  type action = Continue | Exit of int

  val create : unit -> t
  val status : t -> status
  val document_count : t -> int
  val find_document : t -> DocumentUri.t -> Text_document.t option
  val handle : t -> Jsonrpc.Packet.t -> Jsonrpc.Packet.t list * action
  val run : ?input:in_channel -> ?output:out_channel -> unit -> int
end

module Transport = Lsp_transport.Transport

module Make (Language : Language) : Server = struct
  type status = Awaiting_initialize | Running | Shutdown_requested
  type action = Continue | Exit of int

  type open_document = {
    text_document : Text_document.t;
    analysis : Language.analysis;
    diagnostics : Diagnostic.t list;
  }

  module Documents = struct
    module By_uri = Map.Make (String)

    type t = open_document By_uri.t

    let empty = By_uri.empty
    let length = By_uri.cardinal
    let key uri = DocumentUri.to_string uri
    let find uri documents = By_uri.find_opt (key uri) documents

    let add document documents =
      By_uri.add
        (key (Text_document.uri document.text_document))
        document documents

    let remove uri documents =
      (find uri documents, By_uri.remove (key uri) documents)
  end

  type lifecycle =
    | Awaiting
    | Active of Language.state
    | Shutdown of Language.state

  type t = { mutable lifecycle : lifecycle; mutable documents : Documents.t }

  let create () = { lifecycle = Awaiting; documents = Documents.empty }

  let status server =
    match server.lifecycle with
    | Awaiting -> Awaiting_initialize
    | Active _ -> Running
    | Shutdown _ -> Shutdown_requested

  let document_count server = Documents.length server.documents
  let find_open_document server uri = Documents.find uri server.documents

  let find_document server uri =
    Option.map
      (fun document -> document.text_document)
      (find_open_document server uri)

  let language_state server =
    match server.lifecycle with
    | Active state | Shutdown state -> state
    | Awaiting -> failwith "language state accessed before initialize"

  let error code message = Jsonrpc.Response.Error.make ~code ~message ()

  let method_not_found method_ =
    error MethodNotFound ("method not implemented: " ^ method_)

  let server_not_initialized () =
    error ServerNotInitialized "the server has not been initialized"

  let invalid_lifecycle message = error InvalidRequest message

  let log_message ?(type_ = MessageType.Error) message =
    let params = LogMessageParams.create ~type_ ~message in
    let notification = Lsp.Server_notification.LogMessage params in
    Jsonrpc.Packet.Notification
      (Lsp.Server_notification.to_jsonrpc notification)

  let diagnostics_notification document diagnostics =
    let params =
      PublishDiagnosticsParams.create
        ~uri:(Text_document.uri document)
        ~version:(Text_document.version document)
        ~diagnostics ()
    in
    let notification = Lsp.Server_notification.PublishDiagnostics params in
    Jsonrpc.Packet.Notification
      (Lsp.Server_notification.to_jsonrpc notification)

  let publish_diagnostics document =
    diagnostics_notification document.text_document document.diagnostics

  let language_id_is_supported language_id =
    List.exists (( = ) language_id) Language.configuration.language_ids

  let analyze state text_document =
    let analysis = Language.analyze state text_document in
    let diagnostics = Language.diagnostics state analysis text_document in
    { text_document; analysis; diagnostics }

  let add_document server document =
    server.documents <- Documents.add document server.documents

  let remove_document server uri =
    let document, documents = Documents.remove uri server.documents in
    server.documents <- documents;
    document

  let bool_provider = function None -> None | Some _ -> Some (`Bool true)

  let initialize_result () =
    let features = Language.features in
    let completionProvider =
      match features.completion with
      | None -> None
      | Some provider ->
          let triggerCharacters =
            match provider.trigger_characters with
            | [] -> None
            | values -> Some values
          in
          Some (CompletionOptions.create ?triggerCharacters ())
    in
    let textDocumentSync =
      let save = SaveOptions.create ~includeText:true () in
      TextDocumentSyncOptions.create ~openClose:true ~change:Incremental
        ~save:(`SaveOptions save) ()
    in
    let capabilities =
      ServerCapabilities.create
        ~textDocumentSync:(`TextDocumentSyncOptions textDocumentSync)
        ~positionEncoding:PositionEncodingKind.UTF16
        ?hoverProvider:(bool_provider features.hover)
        ?completionProvider
        ?definitionProvider:(bool_provider features.definition)
        ?referencesProvider:(bool_provider features.references)
        ?documentSymbolProvider:(bool_provider features.document_symbols)
        ?documentFormattingProvider:(bool_provider features.formatting)
        ?experimental:Language.configuration.experimental_capabilities ()
    in
    let serverInfo =
      ServerInfo.create ~name:Language.configuration.name
        ?version:Language.configuration.version ()
    in
    InitializeResult.create ~capabilities ~serverInfo ()

  let with_document server uri ~default callback =
    match find_open_document server uri with
    | None -> default
    | Some document ->
        callback (language_state server) document.analysis
          document.text_document

  let handle_running_request : type response.
      t ->
      response Lsp.Client_request.t ->
      (response, Jsonrpc.Response.Error.t) result =
   fun server request ->
    let features = Language.features in
    match request with
    | Lsp.Client_request.Initialize _ ->
        Error (invalid_lifecycle "initialize may only be requested once")
    | Lsp.Client_request.Shutdown ->
        let state = language_state server in
        Option.iter (fun shutdown -> shutdown state) Language.shutdown;
        server.lifecycle <- Shutdown state;
        Ok ()
    | Lsp.Client_request.TextDocumentHover params -> (
        match features.hover with
        | None -> Error (method_not_found "textDocument/hover")
        | Some hover ->
            Ok
              (with_document server params.textDocument.uri ~default:None
                 (fun state analysis document ->
                   hover state analysis document params)))
    | Lsp.Client_request.TextDocumentCompletion params -> (
        match features.completion with
        | None -> Error (method_not_found "textDocument/completion")
        | Some provider ->
            Ok
              (with_document server params.textDocument.uri ~default:None
                 (fun state analysis document ->
                   provider.complete state analysis document params)))
    | Lsp.Client_request.TextDocumentDefinition params -> (
        match features.definition with
        | None -> Error (method_not_found "textDocument/definition")
        | Some definition ->
            Ok
              (with_document server params.textDocument.uri ~default:None
                 (fun state analysis document ->
                   definition state analysis document params)))
    | Lsp.Client_request.TextDocumentReferences params -> (
        match features.references with
        | None -> Error (method_not_found "textDocument/references")
        | Some references ->
            Ok
              (with_document server params.textDocument.uri ~default:None
                 (fun state analysis document ->
                   references state analysis document params)))
    | Lsp.Client_request.DocumentSymbol params -> (
        match features.document_symbols with
        | None -> Error (method_not_found "textDocument/documentSymbol")
        | Some document_symbols ->
            Ok
              (with_document server params.textDocument.uri ~default:None
                 (fun state analysis document ->
                   document_symbols state analysis document params)))
    | Lsp.Client_request.TextDocumentFormatting params -> (
        match features.formatting with
        | None -> Error (method_not_found "textDocument/formatting")
        | Some formatting ->
            Ok
              (with_document server params.textDocument.uri ~default:None
                 (fun state analysis document ->
                   formatting state analysis document params)))
    | Lsp.Client_request.UnknownRequest { meth; params } -> (
        match features.custom_request with
        | None -> Error (method_not_found meth)
        | Some custom_request -> (
            match
              custom_request (language_state server) ~method_:meth ~params
            with
            | None -> Error (method_not_found meth)
            | Some result -> result))
    | _ -> Error (method_not_found "requested LSP method")

  let handle_typed_request : type response.
      t ->
      response Lsp.Client_request.t ->
      (response, Jsonrpc.Response.Error.t) result =
   fun server request ->
    match (status server, request) with
    | Awaiting_initialize, Lsp.Client_request.Initialize params ->
        let state = Language.initialize params in
        server.lifecycle <- Active state;
        Ok (initialize_result ())
    | Awaiting_initialize, _ -> Error (server_not_initialized ())
    | Running, request -> handle_running_request server request
    | Shutdown_requested, _ ->
        Error
          (invalid_lifecycle
             "no requests are accepted after the shutdown request")

  let handle_request server request =
    let result =
      match Lsp.Client_request.of_jsonrpc request with
      | Error message -> Error (error InvalidParams message)
      | Ok (Lsp.Client_request.E typed_request) -> (
          match handle_typed_request server typed_request with
          | Ok response ->
              Ok (Lsp.Client_request.yojson_of_result typed_request response)
          | Error _ as error -> error
          | exception exn ->
              Error
                (error InternalError
                   ("language handler failed: " ^ Printexc.to_string exn)))
    in
    Jsonrpc.Packet.Response Jsonrpc.Response.{ id = request.id; result }

  let did_open server params =
    let item = params.DidOpenTextDocumentParams.textDocument in
    if not (language_id_is_supported item.languageId) then
      [
        log_message
          ("ignoring document with unsupported languageId: "
          ^ Yojson.Safe.to_string (LanguageKind.yojson_of_t item.languageId));
      ]
    else
      let state = language_state server in
      let text_document =
        Text_document.create ~uri:item.uri ~language_id:item.languageId
          ~version:item.version ~text:item.text
      in
      let document = analyze state text_document in
      add_document server document;
      [ publish_diagnostics document ]

  let update_document server document ~version changes =
    match
      Text_document.apply_changes document.text_document ~version changes
    with
    | Error error -> Error (Text_document.string_of_edit_error error)
    | Ok (text_document, applied_changes) ->
        let state = language_state server in
        let analysis =
          match Language.update with
          | None -> Language.analyze state text_document
          | Some update ->
              update state ~previous:document.analysis
                ~old_document:document.text_document ~new_document:text_document
                ~changes:applied_changes
        in
        let diagnostics = Language.diagnostics state analysis text_document in
        Ok { text_document; analysis; diagnostics }

  let did_change server params =
    let identifier = params.DidChangeTextDocumentParams.textDocument in
    match find_open_document server identifier.uri with
    | None ->
        [
          log_message
            ("change received for unopened document "
            ^ DocumentUri.to_string identifier.uri);
        ]
    | Some document
      when identifier.version <= Text_document.version document.text_document ->
        [
          log_message ~type_:MessageType.Warning
            ("ignoring stale document version for "
            ^ DocumentUri.to_string identifier.uri);
        ]
    | Some document -> (
        match
          update_document server document ~version:identifier.version
            params.contentChanges
        with
        | Error message -> [ log_message message ]
        | Ok document ->
            add_document server document;
            [ publish_diagnostics document ])

  let did_close server params =
    let identifier = params.DidCloseTextDocumentParams.textDocument in
    match remove_document server identifier.uri with
    | None -> []
    | Some document ->
        Option.iter
          (fun close ->
            close (language_state server) document.analysis
              document.text_document)
          Language.close_document;
        [ diagnostics_notification document.text_document [] ]

  let did_save server params =
    let identifier = params.DidSaveTextDocumentParams.textDocument in
    match find_open_document server identifier.uri with
    | None -> []
    | Some document -> (
        match params.text with
        | None -> [ publish_diagnostics document ]
        | Some text -> (
            let changes =
              [
                `TextDocumentContentChangeWholeDocument
                  (TextDocumentContentChangeWholeDocument.create ~text);
              ]
            in
            match
              update_document server document
                ~version:(Text_document.version document.text_document)
                changes
            with
            | Error message -> [ log_message message ]
            | Ok document ->
                add_document server document;
                [ publish_diagnostics document ]))

  let notify_language server notification =
    Option.iter
      (fun callback -> callback (language_state server) notification)
      Language.features.notification

  let handle_running_notification server notification =
    match notification with
    | Lsp.Client_notification.TextDocumentDidOpen params ->
        (did_open server params, Continue)
    | TextDocumentDidChange params -> (did_change server params, Continue)
    | TextDocumentDidClose params -> (did_close server params, Continue)
    | DidSaveTextDocument params -> (did_save server params, Continue)
    | Exit -> ([], Exit 1)
    | notification ->
        notify_language server notification;
        ([], Continue)

  let handle_notification server notification =
    match Lsp.Client_notification.of_jsonrpc notification with
    | Error message -> ([ log_message message ], Continue)
    | Ok Exit ->
        let code =
          match status server with
          | Shutdown_requested -> 0
          | Awaiting_initialize | Running -> 1
        in
        ([], Exit code)
    | Ok _ when status server = Awaiting_initialize -> ([], Continue)
    | Ok _ when status server = Shutdown_requested -> ([], Continue)
    | Ok notification -> (
        match handle_running_notification server notification with
        | result -> result
        | exception exn ->
            ( [
                log_message
                  ("language handler failed: " ^ Printexc.to_string exn);
              ],
              Continue ))

  let merge_action left right =
    match (left, right) with
    | Exit code, _ | _, Exit code -> Exit code
    | Continue, Continue -> Continue

  let rec handle server = function
    | Jsonrpc.Packet.Request request ->
        ([ handle_request server request ], Continue)
    | Notification notification -> handle_notification server notification
    | Response _ | Batch_response _ -> ([], Continue)
    | Batch_call calls ->
        let packets, action =
          List.fold_left
            (fun (packets, action) call ->
              let packet =
                match call with
                | `Request request -> Jsonrpc.Packet.Request request
                | `Notification notification -> Notification notification
              in
              let new_packets, new_action = handle server packet in
              ( List.rev_append new_packets packets,
                merge_action action new_action ))
            ([], Continue) calls
        in
        let packets = List.rev packets in
        let responses, notifications =
          List.fold_right
            (fun packet (responses, notifications) ->
              match packet with
              | Jsonrpc.Packet.Response response ->
                  (response :: responses, notifications)
              | packet -> (responses, packet :: notifications))
            packets ([], [])
        in
        let packets =
          match responses with
          | [] -> notifications
          | responses ->
              notifications @ [ Jsonrpc.Packet.Batch_response responses ]
        in
        (packets, action)

  let run ?(input = stdin) ?(output = stdout) () =
    set_binary_mode_in input true;
    set_binary_mode_out output true;
    let server = create () in
    let rec loop () =
      match Transport.read input with
      | None -> 0
      | Some packet -> (
          let packets, action = handle server packet in
          List.iter (Transport.write output) packets;
          match action with Continue -> loop () | Exit code -> code)
    in
    loop ()
end
