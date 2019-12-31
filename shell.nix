{ pkgs ? import <nixpkgs> {} }:

let
  inherit (pkgs) stdenv;
  overlays =
    builtins.fetchTarball https://github.com/anmonteiro/nix-overlays/archive/master.tar.gz;

  ocamlPackages = pkgs.callPackage "${overlays}/ocaml.nix" {
    ocamlPackages = pkgs.ocaml-ng.ocamlPackages_4_09;
  };
  piaf = pkgs.callPackage ./. {};
in
pkgs.mkShell {
  inputsFrom = [ piaf ];
}
