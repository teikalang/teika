# OCaml language server framework

`Language_server.Make` provides the protocol and document-management layer for
an LSP implemented entirely in OCaml. A language adapter supplies analysis and
optional editor features without handling JSON-RPC, server lifecycle, document
storage, or LSP framing.

The implementation keeps protocol dispatch in `language_server.ml`, UTF-16
document editing in `lsp_document.ml`, and synchronous stdio framing in
`lsp_transport.ml`. These remain exposed through the original
`Language_server` API.

## Language adapter

Implement `Language_server.Language` and instantiate the functor:

```ocaml
module My_language = struct
  type state = unit
  type analysis = Ast.t

  include Language_server.Language_defaults

  let configuration =
    Language_server.Configuration.create ~name:"my-language-server"
      ~language_ids:[ Lsp.Types.LanguageKind.Other "my-language" ] ()

  let initialize _params = ()
  let analyze () document = Parser.parse (Language_server.Text_document.text document)
  let diagnostics () analysis _document = Diagnostics.of_ast analysis
end

module Server = Language_server.Make (My_language)

let () = exit (Server.run ())
```

Setting `update` to `None` re-runs `analyze` after changes. Incremental languages
can instead provide `update`; it receives the old and new documents plus ordered
`Text_document.Change` values containing byte offsets, inserted text, and
replaced text.

## Document updates

The server advertises incremental synchronization and accepts both full and
ranged changes. `Text_document.apply_changes`:

- applies every edit sequentially, so later ranges address text produced by
  earlier edits;
- converts the protocol's UTF-16 positions to UTF-8 byte offsets;
- handles LF and CRLF documents;
- retains the original range and optional `rangeLength` for consumers;
- exposes position/offset conversion helpers for parsers and diagnostics.

## Optional features

`Language_server.Features.none` is a convenient starting point. A language can
enable hover, completion (including trigger characters), definition, references,
document symbols, whole-document formatting, custom requests, and notification
hooks by replacing the corresponding fields. Capabilities are derived from the
enabled providers automatically.

The test suite defines the only mock language and covers text updates, UTF-16
positions, normalized incremental edits, capabilities, common requests,
diagnostics, lifecycle behavior, and actual stdio framing:

```console
dune runtest lsp
```

## Teika demo server

The `syntax/` and `typer/` OCaml sources are copied flat into this directory.
`demo_analysis.ml` runs the explicit lexing, parsing, lowering, and vendored
typing pipeline; `syntax_tree_printer.ml` renders the lowered language tree as
precedence-correct Teika source; `demo_hover.ml` handles lowered-tree lookup and
rendering; and `demo_language.ml` is the small LSP adapter. The concrete tree is
only an internal parser-to-lowering representation and is never printed by an
editor feature. Parser, lowering, and type-check failures become diagnostics.
The adapter uses the framework's full-analysis fallback after both full and
partial document updates.

The demo frontend covers the stable typed core described across the repository
notes: holes and `Type`, declarations and definitions, dependent function types
and lambdas, application, annotations, `self`/`fix` and `.@`, units, dependent
pairs, pair values, and `.0`/`.1`. It also recognizes the documented concrete
forms for intersections, unions, strings, numbers, and implicit braces, then
reports a lowering diagnostic where the vendored typed core has no matching
construct instead of silently assigning them invented semantics.

From the repository root, run the server over stdio with:

```console
dune exec lsp/demo_lsp.exe
```

Clients should open demo documents with the language ID `teika`. Demo tests
cover lexing, parser and typer diagnostics, confirmation that the Boolean
example reaches the vendored typer, lowered-syntax hover, hover after a ranged
partial update, and completion. `examples/bool.tei` deliberately produces an
unbound-variable diagnostic from the typer; `examples/type.tei` and
`examples/core.tei` are accepted without diagnostics.

## Teika VS Code extension

The extension lives in `lsp/vscode/`. Its activation and client setup are
written in `vscode_extension.ml`, compiled by Melange, and bundled with the
standard VS Code language client:

```console
npm --prefix lsp/vscode install
npm --prefix lsp/vscode run build
code-insiders --new-window --extensionDevelopmentPath="$PWD/lsp/vscode" "$PWD"
```

`vscode_bindings.ml` contains the typed editor bindings and `vscode_probe.ml`
contains the extension-host integration check. Dune compiles both with Melange;
no JavaScript source is maintained in the repository:

```console
nix develop -c dune build --build-dir "$PWD/lsp/_build" @lsp/vscode-probe
code-insiders --new-window --extensionDevelopmentPath="$PWD/lsp/vscode" \
  --extensionTestsPath="$PWD/lsp/_build/default/lsp/vscode_probe/lsp/vscode_probe.js" \
  "$PWD"
```

The probe checks that `type.tei` and `core.tei` have no diagnostics and that
`bool.tei` has working hover, completion, and the expected vendored-typer
diagnostic. Hover assertions require printed lowered Teika syntax and reject
concrete-tree constructor names.
