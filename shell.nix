{ pkgs ? import <nixpkgs> {} }:

let
  piaf = pkgs.callPackage ./. {};
in
pkgs.mkShell {
  inputsFrom = [ piaf ];
}
