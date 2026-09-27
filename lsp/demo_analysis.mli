(** Parsing and type analysis for the vendored demo language. *)

type phase = Lexing | Parsing | Lowering | Typing
type issue
type t

val run : string -> t
val concrete : t -> Lsp_demo_syntax.Ctree.term option
val lowered : t -> Lsp_demo_syntax.Lparser.Syntax_tree.term option
val issues : t -> issue list
val issue_phase : issue -> phase
val issue_message : issue -> string
val pp_phase : Format.formatter -> phase -> unit
val equal_phase : phase -> phase -> bool

val diagnostics :
  Language_server.Text_document.t -> t -> Lsp.Types.Diagnostic.t list
