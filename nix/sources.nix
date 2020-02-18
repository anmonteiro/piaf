{ ocamlVersion ? "4_09" }:

let
  overlays = builtins.fetchTarball {
    url = https://github.com/anmonteiro/nix-overlays/archive/be125f0.tar.gz;
    sha256 = "0vy12r7pg6pxw17x9v4kn19fhz595wwxi8bszw548yj8b536gkxg";
  };
  pkgs = import <nixpkgs> {
    overlays = [
      (import overlays)
      (self: super: {
        ocamlPackages = super.ocaml-ng."ocamlPackages_${ocamlVersion}".overrideScope'
            (super.callPackage "${overlays}/ocaml" {});
      })
    ];
  };

in
  pkgs
