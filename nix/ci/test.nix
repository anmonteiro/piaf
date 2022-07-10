{ ocamlVersion }:

let
  lock = builtins.fromJSON (builtins.readFile ./../../flake.lock);
  src = fetchGit {
    url = with lock.nodes.nixpkgs.locked;"https://github.com/${owner}/${repo}";
    inherit (lock.nodes.nixpkgs.locked) rev;
  };
  pkgs = import src {
    extraOverlays = [
      (self: super: {
        ocamlPackages = super.ocaml-ng."ocamlPackages_${ocamlVersion}";
      })
    ];
  };

  inherit (pkgs) lib stdenv fetchTarball ocamlPackages callPackage;
  piafPkgs = callPackage ./.. { doCheck = true; };

  test = pkg:
    let piafDrvs = lib.filterAttrs (_: value: lib.isDerivation value) pkg;
    in
    stdenv.mkDerivation {
      name = "piaf-tests";
      src = lib.filterGitSource {
        src = ./../..;
        dirs = [ "lib" "lib_test" "examples" ];
        files = [ ".ocamlformat" "piaf.opam" "dune-project" "dune" ];
      };
      dontBuild = true;
      installPhase = ''
        touch $out
      '';
      buildInputs = (lib.attrValues piafDrvs) ++ (with ocamlPackages; [
        ocaml
        dune
        findlib
        ocamlformat
      ]);
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
