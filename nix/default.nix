{ pkgsPath ? <nixpkgs> }:

let
  pkgs = import pkgsPath {};
  inherit (pkgs) lib;
  overlays = builtins.fetchTarball https://github.com/anmonteiro/nix-overlays/archive/master.tar.gz;

  ocamlPackages = pkgs.ocaml-ng.ocamlPackages_4_09.overrideScope'
    (pkgs.callPackage "${overlays}/ocaml.nix" { });

in
  {
    native = pkgs.callPackage ./generic.nix {
      inherit ocamlPackages;
    };

    musl64 =
      let pkgs = import "${overlays}/static.nix" {
        inherit pkgsPath;
        ocamlVersion = "4_09";
      };
      zlib = pkgs.zlib.override {
        static = true;
        shared = false;
        splitStaticOutput = false;

        # Don’t use new stdenv zlib because
        # it doesn’t like the --disable-shared flag
        # stdenv = super.stdenv;
      };
      in
      pkgs.callPackage ./generic.nix {
        ocamlPackages = pkgs.ocaml-ng.ocamlPackages_4_09.overrideScope' (oself: osuper: {
          camlzip = osuper.camlzip.override { inherit zlib; };
        });
      };
  }
