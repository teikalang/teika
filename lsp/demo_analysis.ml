module Source_location = Location
open Lsp.Types
module Syntax = Lsp_demo_syntax
module Vendored_typer = Lsp_demo_typer.Typer

type phase = Lexing | Parsing | Lowering | Typing

type issue = {
  phase : phase;
  location : Source_location.t option;
  message : string;
}

type t = {
  concrete : Syntax.Ctree.term option;
  lowered : Syntax.Lparser.Syntax_tree.term option;
  issues : issue list;
}

let concrete analysis = analysis.concrete
let lowered analysis = analysis.lowered
let issues analysis = analysis.issues
let issue_phase issue = issue.phase
let issue_message issue = issue.message
let equal_phase = ( = )

let pp_phase formatter = function
  | Lexing -> Format.pp_print_string formatter "lexing"
  | Parsing -> Format.pp_print_string formatter "parsing"
  | Lowering -> Format.pp_print_string formatter "lowering"
  | Typing -> Format.pp_print_string formatter "typing"

let exception_message = function
  | Failure message -> message
  | exception_ -> Printexc.to_string exception_

let root_location (Syntax.Ctree.CTerm { loc; _ }) = loc
let issue ?location phase message = { phase; location; message }

let protect ?location phase operation =
  match operation () with
  | value -> Ok value
  | exception exn -> Error (issue ?location phase (exception_message exn))

let type_check lowered =
  let module Typer = Vendored_typer in
  let context = Typer.Elaborate.Context { vars = []; at = Typer.Level.zero } in
  let term = Typer.Elaborate.elab_term context lowered in
  let expected = Typer.Check_tree.t_new_flex ~at:Typer.Level.zero () in
  Typer.Check.check term expected

let analyze_concrete concrete =
  let location = Some (root_location concrete) in
  match
    protect ?location Lowering (fun () -> Syntax.Lparser.parse_term concrete)
  with
  | Error issue ->
      { concrete = Some concrete; lowered = None; issues = [ issue ] }
  | Ok lowered ->
      let issues =
        match protect ?location Typing (fun () -> type_check lowered) with
        | Ok () -> []
        | Error issue -> [ issue ]
      in
      { concrete = Some concrete; lowered = Some lowered; issues }

let run text =
  match Syntax.Demo_lexer.term_opt_from_string text with
  | None -> { concrete = None; lowered = None; issues = [] }
  | Some concrete -> analyze_concrete concrete
  | exception Syntax.Demo_lexer.Lexer_error { location; lexeme } ->
      {
        concrete = None;
        lowered = None;
        issues =
          [
            issue ~location Lexing
              (Format.asprintf "unexpected token %S" lexeme);
          ];
      }
  | exception Syntax.Demo_lexer.Parser_error { location } ->
      {
        concrete = None;
        lowered = None;
        issues = [ issue ~location Parsing "syntax error" ];
      }

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

let diagnostic document issue =
  let range =
    Option.fold
      ~none:(Language_server.Text_document.full_range document)
      ~some:(range_of_location document)
      issue.location
  in
  Diagnostic.create ~range ~severity:DiagnosticSeverity.Error
    ~source:"teika-demo" ~message:(`String issue.message) ()

let diagnostics document analysis =
  List.map (diagnostic document) analysis.issues
