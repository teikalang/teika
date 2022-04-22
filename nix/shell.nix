{ pkgs, not-named-yet }:

with pkgs; mkShell {
  inputsFrom = [ not-named-yet ];
  packages = [
    # Make developer life easier
    # formatters
    nixfmt
    ocamlformat
  ] ++ (with ocamlPackages; [
    # OCaml developer tooling
    ocaml
    dune_3
    ocaml-lsp
    ocamlformat-rpc
  ]);
}
