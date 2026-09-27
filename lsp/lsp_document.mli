(** An open text document with LSP-compatible UTF-16 position handling. *)

type t

type edit_error =
  | Invalid_position of Lsp.Types.Position.t
  | Invalid_range of Lsp.Types.Range.t
  | Invalid_offset of int

module Change : sig
  type t

  val range : t -> Lsp.Types.Range.t option
  val range_length : t -> int option
  val start_offset : t -> int
  val end_offset : t -> int
  val inserted_text : t -> string
  val replaced_text : t -> string
end

val create :
  uri:Lsp.Types.DocumentUri.t ->
  language_id:Lsp.Types.LanguageKind.t ->
  version:int ->
  text:string ->
  t

val uri : t -> Lsp.Types.DocumentUri.t
val language_id : t -> Lsp.Types.LanguageKind.t
val version : t -> int
val text : t -> string

val offset_of_position : t -> Lsp.Types.Position.t -> (int, edit_error) result
(** Convert an LSP UTF-16 position to a byte offset. Characters beyond the end
    of an existing line are clamped as required by the LSP. *)

val position_of_offset : t -> int -> (Lsp.Types.Position.t, edit_error) result
(** Convert a UTF-8 byte offset to an LSP UTF-16 position. *)

val full_range : t -> Lsp.Types.Range.t

val apply_changes :
  t ->
  version:int ->
  Lsp.Types.TextDocumentContentChangeEvent.t list ->
  (t * Change.t list, edit_error) result
(** Apply changes sequentially, as specified by LSP. Returned change offsets are
    relative to the text immediately before that individual change. This is
    suitable for feeding an incremental parser. *)

val string_of_edit_error : edit_error -> string
