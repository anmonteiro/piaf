{ pkgsPath ? <nixpkgs> }:

let
  pkgs = import pkgsPath {};
  overlays = builtins.fetchTarball {
    url = https://github.com/anmonteiro/nix-overlays/archive/d2d883d84.tar.gz;
    sha256 = "1raq4wnv8h0v23kar79hra6xffkfl281zw4vzvyfr1fwgwwk7bx3";
  };

  ocamlPackages = pkgs.ocaml-ng.ocamlPackages_4_09.overrideScope'
    (pkgs.callPackage "${overlays}/ocaml" {});
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
      };
      in
      pkgs.callPackage ./generic.nix {
        static = true;
        ocamlPackages = pkgs.ocaml-ng.ocamlPackages_4_09.overrideScope' (oself: osuper: {
          camlzip = osuper.camlzip.override { inherit zlib; };
        });
      };
  }
