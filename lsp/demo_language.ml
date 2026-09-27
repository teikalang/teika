open Lsp.Types
module Analysis = Demo_analysis
module Hover_provider = Demo_hover
module Printer = Lsp_demo_syntax.Syntax_tree_printer

module Completion = struct
  let item ?detail ?insertText label =
    CompletionItem.create ~label ?detail ?insertText
      ~kind:CompletionItemKind.Snippet ()

  let provide () =
    Some
      (`List
         [
           item "_" ~detail:"hole";
           item "()" ~detail:"parenthesized term" ~insertText:"($1)";
           item "[]" ~detail:"pair or pattern" ~insertText:"[$1, $2]";
           item "{}" ~detail:"braced term" ~insertText:"{$1}";
         ])
end

module Language = struct
  type state = unit
  type analysis = Analysis.t

  include Language_server.Language_defaults

  let configuration =
    Language_server.Configuration.create ~name:"Teika demo language server"
      ~version:"0.1.0"
      ~language_ids:[ LanguageKind.Other "teika" ]
      ()

  let initialize _params = ()

  let analyze () document =
    Analysis.run (Language_server.Text_document.text document)

  let diagnostics () analysis document = Analysis.diagnostics document analysis

  let hover () analysis document (params : HoverParams.t) =
    Hover_provider.at_position analysis document params.position

  let complete () _analysis _document _params = Completion.provide ()

  let features : (state, analysis) Language_server.Features.t =
    {
      Language_server.Features.none with
      hover = Some hover;
      completion = Some { trigger_characters = []; complete };
    }
end

module Server = Language_server.Make (Language)
