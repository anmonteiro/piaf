{ pkgsPath ? <nixpkgs> }:

let
  pkgs = import pkgsPath {};

  gitignoreSrc = pkgs.fetchFromGitHub {
    owner = "hercules-ci";
    repo = "gitignore";
    rev = "7415c4f";
    sha256 = "1zd1ylgkndbb5szji32ivfhwh04mr1sbgrnvbrqpmfb67g2g3r9i";
  };
  inherit (import gitignoreSrc { inherit (pkgs) lib; }) gitignoreSource;

  overlays = builtins.fetchTarball {
    url = https://github.com/anmonteiro/nix-overlays/archive/23539c4.tar.gz;
    sha256 = "1y1bip3wnvj92mcxniy71ajyb48r5navw09cph37i4dzg4vf8zid";
  };

  ocamlPackages = pkgs.ocaml-ng.ocamlPackages_4_09.overrideScope'
    (pkgs.callPackage "${overlays}/ocaml" {});
in
  {
    native = pkgs.callPackage ./generic.nix {
      inherit ocamlPackages gitignoreSource;
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
        inherit gitignoreSource;
        ocamlPackages = pkgs.ocaml-ng.ocamlPackages_4_09.overrideScope' (oself: osuper: {
          camlzip = osuper.camlzip.override { inherit zlib; };
        });
      };
  }
