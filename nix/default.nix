{ pkgs ? import ./sources.nix {} }:

let
  pkgsCross = pkgs.pkgsCross.musl64.pkgsStatic;
in
  {
    native = pkgs.callPackage ./generic.nix {};

    musl64 = pkgsCross.callPackage ./generic.nix {
      static = true;
      ocamlPackages = pkgsCross.ocaml-ng.ocamlPackages_4_09;
    };
  }
