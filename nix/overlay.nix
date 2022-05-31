self: super:
with super; {
  # ocaml-ng = builtins.mapAttrs (_: ocamlVersion: ocamlVersion) super.ocaml-ng;
  # TODO: this is clearly not right, I should be overriding only 4_14
  ocaml-ng = ocaml-ng // (with ocaml-ng; {
    ocamlPackages_4_14 = ocamlPackages_4_14.overrideScope'
      (_: super: {
        ocaml-lsp = super.ocaml-lsp.overrideAttrs (_: {
          src = fetchFromGitHub {
            owner = "EduardoRFS";
            repo = "ocaml-lsp";
            rev = "eeb5ff15c6a3e78c897c6f787ce8ebb4ffe10934";
            sha256 = "+Ax6/fh0Kdlku1XWU2dukerGvvzznR4qjDsm9zG5cLg=";
            fetchSubmodules = true;
          };
        });
      });
  });
}
