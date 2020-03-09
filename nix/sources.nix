{ ocamlVersion ? "4_10" }:

let
  overlays = builtins.fetchTarball {
    url = https://github.com/anmonteiro/nix-overlays/archive/5d6ac00.tar.gz;
    sha256 = "0dl92riggcz2zxrlrkqh51rbjz2kc5k8ad2wri1y2ivk3b6qa37n";
  };

in

  import "${overlays}/sources.nix" {
    overlays = [
      (import overlays)
      (self: super: {
        ocamlPackages = super.ocaml-ng."ocamlPackages_${ocamlVersion}".overrideScope'
            (super.callPackage "${overlays}/ocaml" {});
      })
    ];
  }
