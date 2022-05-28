{ pkgs, teika }:

with pkgs; with ocamlPackages; mkShell {
  inputsFrom = [ teika ];
  packages = [
    # Make developer life easier
    # formatters
    nixfmt
    ocamlformat

    # OCaml developer tooling
    ocaml
    dune_3
    ocaml-lsp
    ocamlformat-rpc
  ];
}
