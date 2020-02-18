{ ocamlVersion ? "4_09" }:

let
  overlays = builtins.fetchTarball {
    url = https://github.com/anmonteiro/nix-overlays/archive/7eb9624.tar.gz;
    sha256 = "1mdb09igbnp7gcrfmffb19mn4yxaqxvb94lr8gr63hbb67cnfray";
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
