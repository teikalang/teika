(** Reusable, synchronous Language Server Protocol infrastructure. *)

module Text_document : module type of Lsp_document

module Configuration : sig
  type t

  val create :
    name:string ->
    ?version:string ->
    language_ids:Lsp.Types.LanguageKind.t list ->
    ?experimental_capabilities:Yojson.Safe.t ->
    unit ->
    t
end

module Features : sig
  type completion_result =
    [ `CompletionList of Lsp.Types.CompletionList.t
    | `List of Lsp.Types.CompletionItem.t list ]
    option

  type document_symbol_result =
    [ `DocumentSymbol of Lsp.Types.DocumentSymbol.t list
    | `SymbolInformation of Lsp.Types.SymbolInformation.t list ]
    option

  type ('state, 'analysis) completion_provider = {
    trigger_characters : string list;
    complete :
      'state ->
      'analysis ->
      Text_document.t ->
      Lsp.Types.CompletionParams.t ->
      completion_result;
  }

  type ('state, 'analysis) t = {
    hover :
      ('state ->
      'analysis ->
      Text_document.t ->
      Lsp.Types.HoverParams.t ->
      Lsp.Types.Hover.t option)
      option;
    completion : ('state, 'analysis) completion_provider option;
    definition :
      ('state ->
      'analysis ->
      Text_document.t ->
      Lsp.Types.DefinitionParams.t ->
      Lsp.Types.Locations.t option)
      option;
    references :
      ('state ->
      'analysis ->
      Text_document.t ->
      Lsp.Types.ReferenceParams.t ->
      Lsp.Types.Location.t list option)
      option;
    document_symbols :
      ('state ->
      'analysis ->
      Text_document.t ->
      Lsp.Types.DocumentSymbolParams.t ->
      document_symbol_result)
      option;
    formatting :
      ('state ->
      'analysis ->
      Text_document.t ->
      Lsp.Types.DocumentFormattingParams.t ->
      Lsp.Types.TextEdit.t list option)
      option;
    custom_request :
      ('state ->
      method_:string ->
      params:Jsonrpc.Structured.t option ->
      (Yojson.Safe.t, Jsonrpc.Response.Error.t) result option)
      option;
    notification : ('state -> Lsp.Client_notification.t -> unit) option;
  }

  val none : ('state, 'analysis) t
end

(** Optional language callbacks with no-op defaults. [include] this module in a
    language adapter, then override only the values that adapter needs. *)
module Language_defaults : sig
  val update :
    ('state ->
    previous:'analysis ->
    old_document:Text_document.t ->
    new_document:Text_document.t ->
    changes:Text_document.Change.t list ->
    'analysis)
    option

  val close_document : ('state -> 'analysis -> Text_document.t -> unit) option
  val shutdown : ('state -> unit) option
  val features : ('state, 'analysis) Features.t
end

module type Language = sig
  type state
  type analysis

  val configuration : Configuration.t

  val initialize : Lsp.Types.InitializeParams.t -> state
  (** Called once while handling [initialize]. *)

  val analyze : state -> Text_document.t -> analysis
  (** Full analysis used on open and as the fallback when [update] is [None]. *)

  val update :
    (state ->
    previous:analysis ->
    old_document:Text_document.t ->
    new_document:Text_document.t ->
    changes:Text_document.Change.t list ->
    analysis)
    option
  (** Optional incremental analysis. Changes are ordered and their offsets are
      relative to the state produced by preceding changes. *)

  val diagnostics :
    state -> analysis -> Text_document.t -> Lsp.Types.Diagnostic.t list

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
  val find_document : t -> Lsp.Types.DocumentUri.t -> Text_document.t option
  val handle : t -> Jsonrpc.Packet.t -> Jsonrpc.Packet.t list * action
  val run : ?input:in_channel -> ?output:out_channel -> unit -> int
end

module Make (_ : Language) : Server
