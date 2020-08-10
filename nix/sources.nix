{ ocamlVersion ? "4_10" }:

let
  overlays =
    builtins.fetchTarball
      https://github.com/anmonteiro/nix-overlays/archive/a7d2d27.tar.gz;

in

  import "${overlays}/sources.nix" {
    overlays = [
      (import overlays)
      (self: super: {
        ocamlPackages = super.ocaml-ng."ocamlPackages_${ocamlVersion}".overrideScope'
            (super.callPackage "${overlays}/ocaml" {});

        pkgsCross.musl64.pkgsStatic =
          super.pkgsCross.musl64.pkgsStatic.appendOverlays
            (import "${overlays}/static/overlays.nix" {
              inherit ocamlVersion;
              lib = super.lib;
              pkgsNative = self;
            });
      })
    ];
  }
