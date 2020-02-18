{ pkgs ? import ./sources.nix {}, doCheck ? false }:

let
  pkgsCross = pkgs.pkgsCross.musl64.pkgsStatic;
in
  {
    native = pkgs.callPackage ./generic.nix {
      inherit doCheck;
    };

    musl64 = pkgsCross.callPackage ./generic.nix {
      static = true;
      inherit doCheck;
      ocamlPackages = pkgsCross.ocaml-ng.ocamlPackages_4_09;
    };
  }
