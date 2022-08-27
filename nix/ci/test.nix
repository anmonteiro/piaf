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
  nix-filter-src = fetchGit {
    url = with lock.nodes.nix-filter.locked; "https://github.com/${owner}/${repo}";
    inherit (lock.nodes.nix-filter.locked) rev;
    # inherit (lock.nodes.nixpkgs.original) ref;
    allRefs = true;
  };
  nix-filter = import "${nix-filter-src}";

  inherit (pkgs) lib stdenv fetchTarball ocamlPackages callPackage;
  piafPkgs = callPackage ./.. {
    doCheck = true;
    inherit nix-filter;
  };

  test = pkg:
    let piafDrvs = lib.filterAttrs (_: value: lib.isDerivation value) pkg;
    in
    stdenv.mkDerivation {
      name = "piaf-tests";
      src = with nix-filter; filter {
        root = ./..;
        include = [ ".ocamlformat" "piaf.opam" "piaf-lwt.opam" "dune-project" "dune" ] ++ (builtins.map inDirectory [
          "lib"
          "lib_test"
          "multipart"
          "multipart_test"
          "lib-lwt"
          "lib-lwt_test"
          "multipart-lwt"
          "multipart-lwt_test"
          "stream"
          "examples"
        ]);
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
