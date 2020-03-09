{ pkgs ? import ./sources.nix {}, doCheck ? false, ocamlVersion ? "4_09" }:

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
      ocamlPackages = pkgsCross.ocaml-ng."ocamlPackages_${ocamlVersion}";
    };
  }
