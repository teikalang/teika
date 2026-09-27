module Source_location = Location
open Lsp.Types
module Syntax = Lsp_demo_syntax
module Tree = Syntax.Lparser.Syntax_tree

type node = Term of Tree.term | Pattern of Tree.pat

let position_of_offset document offset =
  match Language_server.Text_document.position_of_offset document offset with
  | Ok position -> position
  | Error _ -> Position.create ~line:0 ~character:0

let range_of_location document location =
  if Source_location.is_none location then
    Language_server.Text_document.full_range document
  else
    let start = position_of_offset document location.loc_start.pos_cnum in
    let end_ = position_of_offset document location.loc_end.pos_cnum in
    Range.create ~start ~end_

let location_contains location offset =
  (not (Source_location.is_none location))
  && location.loc_start.pos_cnum <= offset
  && offset < location.loc_end.pos_cnum

let node_location = function
  | Term (Tree.Term { loc; _ }) | Pattern (Tree.Pat { loc; _ }) -> loc

let term_children (Tree.Term { head; _ }) =
  match head with
  | T_hole | T_type | T_var _ | TF_unit | TI_unit -> []
  | T_def { bound; body } -> [ Pattern bound; Term body ]
  | T_let { bound; arg; body } -> [ Pattern bound; Term arg; Term body ]
  | TF_pi { bound; body }
  | TI_lambda { bound; body }
  | TF_self { bound; body }
  | TI_fix { bound; body } ->
      [ Pattern bound; Term body ]
  | TE_apply { funct; arg } -> [ Term funct; Term arg ]
  | TE_annot { value; annot } -> [ Term value; Term annot ]
  | TE_unfold { fix } -> [ Term fix ]
  | TE_drop { arg; body } -> [ Term arg; Term body ]
  | TF_sigma { fst; snd } -> [ Pattern fst; Term snd ]
  | TI_pair { fst; snd } -> [ Term fst; Term snd ]
  | TE_fst { pair } | TE_snd { pair } -> [ Term pair ]

let pattern_children (Tree.Pat { head; _ }) =
  match head with
  | P_var _ | P_wildcard | P_unit -> []
  | P_annot { pat; annot } -> [ Pattern pat; Term annot ]
  | P_pair { fst; snd } -> [ Pattern fst; Pattern snd ]
  | P_apply { funct; arg } -> [ Pattern funct; Pattern arg ]

let children = function
  | Term term -> term_children term
  | Pattern pattern -> pattern_children pattern

let rec node_at_offset offset node =
  if not (location_contains (node_location node) offset) then None
  else
    match List.find_map (node_at_offset offset) (children node) with
    | Some _ as child -> child
    | None -> Some node

let term_kind (Tree.Term { head; _ }) =
  match head with
  | T_hole -> "hole"
  | T_type -> "universe"
  | T_var _ -> "variable"
  | T_def _ -> "declaration"
  | T_let _ -> "definition"
  | TF_pi _ -> "dependent function type"
  | TI_lambda _ -> "function"
  | TE_apply _ -> "application"
  | TE_annot _ -> "annotation"
  | TF_self _ -> "self type"
  | TI_fix _ -> "fixed point"
  | TE_unfold _ -> "unfold"
  | TF_unit -> "unit type"
  | TI_unit -> "unit value"
  | TE_drop _ -> "unit elimination"
  | TF_sigma _ -> "dependent pair type"
  | TI_pair _ -> "pair"
  | TE_fst _ -> "first projection"
  | TE_snd _ -> "second projection"

let pattern_kind (Tree.Pat { head; _ }) =
  match head with
  | P_annot _ -> "annotated pattern"
  | P_var _ -> "binding"
  | P_wildcard -> "wildcard"
  | P_unit -> "unit pattern"
  | P_pair _ -> "pair pattern"
  | P_apply _ -> "function binding"

let node_kind = function
  | Term term -> term_kind term
  | Pattern pattern -> pattern_kind pattern

let node_text = function
  | Term term -> Syntax.Syntax_tree_printer.to_string term
  | Pattern pattern -> Syntax.Syntax_tree_printer.pattern_to_string pattern

let render document node =
  let location = node_location node in
  let value =
    Format.asprintf "**%s**\n\n```teika\n%s\n```" (node_kind node)
      (node_text node)
  in
  let contents = MarkupContent.create ~kind:MarkupKind.Markdown ~value in
  Hover.create ~contents:(`MarkupContent contents)
    ~range:(range_of_location document location)
    ()

let at_position analysis document position =
  match
    ( Demo_analysis.lowered analysis,
      Language_server.Text_document.offset_of_position document position )
  with
  | Some lowered, Ok offset ->
      node_at_offset offset (Term lowered) |> Option.map (render document)
  | None, _ | _, Error _ -> None
