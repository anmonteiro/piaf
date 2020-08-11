{ ocamlVersion }:

let
  pkgs = import ../sources.nix { inherit ocamlVersion; };
  inherit (pkgs) lib stdenv fetchTarball ocamlPackages;

  piafPkgs =  (import ./.. {
    inherit pkgs;
    doCheck = true;
  });

  test = pkg:
  let piafDrvs = lib.filterAttrs (_: value: lib.isDerivation value) pkg;
  in
  stdenv.mkDerivation {
    name = "piaf-tests";
    src = lib.filterGitSource {
      src = ./../..;
      files = [ ".ocamlformat" ];
    };
    dontBuild = true;
    installPhase = ''
      touch $out
    '';
    buildInputs = (lib.attrValues piafDrvs) ++ (with ocamlPackages; [ ocaml dune findlib pkgs.ocamlformat ]);
    doCheck = true;
    checkPhase = ''
      # Check code is formatted with OCamlformat
      dune build @fmt
    '';
  };
in
  {
    native = test piafPkgs.native;
    musl64 = test piafPkgs.musl64;
  }
