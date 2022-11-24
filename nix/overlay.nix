self: super:
with super; {
  # ocaml-ng = builtins.mapAttrs (_: ocamlVersion: ocamlVersion) super.ocaml-ng;
  # TODO: this is clearly not right, I should be overriding only 4_14
  ocaml-ng = ocaml-ng // (with ocaml-ng; {
    ocamlPackages_5_0 = ocamlPackages_5_0.overrideScope'
      (_: super: {
        ocaml-lsp = super.ocaml-lsp.overrideAttrs (_: {
          src = fetchFromGitHub {
            owner = "EduardoRFS";
            repo = "ocaml-lsp";
            rev = "72202cd2bb9ff65845b623d20f513f6ed5b82ab8";
            sha256 = "49E7L50i9RZTrQDPmdqyeOaBSXjRo/ijjrsj9oztduM=";
            fetchSubmodules = true;
          };
        });
        jsonrpc = super.jsonrpc.overrideAttrs (_: {
          src = fetchFromGitHub {
            owner = "EduardoRFS";
            repo = "ocaml-lsp";
            rev = "72202cd2bb9ff65845b623d20f513f6ed5b82ab8";
            sha256 = "49E7L50i9RZTrQDPmdqyeOaBSXjRo/ijjrsj9oztduM=";
            fetchSubmodules = true;
          };
        });
      });
  });
}
