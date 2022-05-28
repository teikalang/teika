self: super:
with super; {
  # ocaml-ng = builtins.mapAttrs (_: ocamlVersion: ocamlVersion) super.ocaml-ng;
  # TODO: this is clearly not right, I should be overriding only 4_14
  ocaml-ng = ocaml-ng // (with ocaml-ng; {
    ocamlPackages_4_14 = ocamlPackages_4_14.overrideScope'
      (_: super: {
        ocaml-lsp = super.ocaml-lsp.overrideAttrs (_: {
          src = fetchurl {
            url = "https://github.com/ocaml/ocaml-lsp/releases/download/1.11.6/jsonrpc-1.11.6.tbz";
            sha256 = "50b546ced5332c4a038bcf68b65b7888cb8e61aebe102e8c80b23a4c5899bbbb";
          };
        });
      });
  });
}
