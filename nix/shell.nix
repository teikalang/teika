{ pkgs, teika }:

with pkgs;
with ocaml-ng.ocamlPackages_5_5;
pkgs.mkShell {
  inputsFrom = [
    teika
  ];

  packages = [
    # Nix tooling
    nixfmt

    # OCaml developer tooling
    dune_3
    menhir
    ocaml
    ocaml-lsp
    ocamlformat
    utop
  ];
}
