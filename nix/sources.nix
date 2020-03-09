{ ocamlVersion ? "4_09" }:

let
  overlays = builtins.fetchTarball {
    url = https://github.com/anmonteiro/nix-overlays/archive/4799382.tar.gz;
    sha256 = "0kah0fnf5v2aanwj5j5579n1zajay3bgczdl883sgw992af5mgk2";
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
