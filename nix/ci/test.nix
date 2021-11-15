{ ocamlVersion }:

let
  lock = builtins.fromJSON (builtins.readFile ./../../flake.lock);
  src = fetchGit {
    url = with lock.nodes.nixpkgs.locked;"https://github.com/${owner}/${repo}";
    inherit (lock.nodes.nixpkgs.locked) rev;
    # inherit (lock.nodes.nixpkgs.original) ref;
  };
  pkgs = import "${src}/boot.nix" {
    overlays = [
      (import src)
      (self: super: {
        ocamlPackages = super.ocaml-ng."ocamlPackages_${ocamlVersion}";

        pkgsCross.musl64 = super.pkgsCross.musl64 // {
          ocamlPackages = super.pkgsCross.musl64.ocaml-ng."ocamlPackages_${ocamlVersion}";
        };
      })
    ];
  };

  inherit (pkgs) lib stdenv fetchTarball ocamlPackages;

  piafPkgs = (import ./.. {
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
        dirs = [ "lib" "lib_test" "examples" ];
        files = [ ".ocamlformat" "piaf.opam" "dune-project" "dune" ];
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
