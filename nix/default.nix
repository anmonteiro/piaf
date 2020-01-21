{ sources ? import ./sources.nix {} }:

let
  inherit (sources) pkgs gitignoreSource;
  pkgsCross = pkgs.pkgsCross.musl64.pkgsStatic;
in
  {
    native = pkgs.callPackage ./generic.nix {
      inherit gitignoreSource;
    };

    musl64 = pkgsCross.callPackage ./generic.nix {
      static = true;
      inherit gitignoreSource;
      ocamlPackages = pkgsCross.ocaml-ng.ocamlPackages_4_09;
    };
  }
