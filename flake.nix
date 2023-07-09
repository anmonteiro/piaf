{
  description = "Piaf Nix Flake";

  inputs.nix-filter.url = "github:numtide/nix-filter";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs = {
    url = "github:nix-ocaml/nix-overlays";
    inputs.flake-utils.follows = "flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, nix-filter }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages."${system}".extend (self: super: {
          ocamlPackages = super.ocaml-ng.ocamlPackages_5_1.overrideScope' (oself: osuper: {
            gluten = osuper.gluten.overrideAttrs (_: {
              src = super.fetchFromGitHub {
                owner = "anmonteiro";
                repo = "gluten";
                rev = "5d0d3134c69108a4eda0a86ca39ddfe8fa69bf3e";
                hash = "sha256-BPSLfgnO0uvLozrKbI2wGmTYgTMyY0YAaX5RYXw+f40=";
              };
            });

            hpack = osuper.hpack.overrideAttrs (_: {
              src = super.fetchFromGitHub {
                owner = "anmonteiro";
                repo = "ocaml-h2";
                rev = "4c97333c4c0f1dbce698da7d5f50217b43aadd8b";
                hash = "sha256-aWQwavWRhXly2Ic+blaaBkydM0CiU7I3nenxhfD4/7k=";
              };
            });

            eio-ssl = osuper.eio-ssl.overrideAttrs (_: {
              src = super.fetchFromGitHub {
                owner = "anmonteiro";
                repo = "eio-ssl";
                rev = "4492a4acce6720f9deec49448b01931d862a68e4";
                hash = "sha256-8iMwzqmql4JUMLrp5dB1TAg6y2Irzr4GGlvESaFNm/o=";
              };
            });

            httpaf = osuper.httpaf.overrideAttrs (_: {
              src = super.fetchFromGitHub {
                owner = "anmonteiro";
                repo = "httpaf";
                rev = "6c1f68fb56662e46ec94a16b1b42e3e92252359e";
                hash = "sha256-WoCm6QNQO/yxZhNf+TUaAoaQ+qUmEEvYfnOMooG1VkI=";
              };
            });
          });
        });
      in
      rec {
        packages = {
          native = pkgs.callPackage ./nix {
            nix-filter = nix-filter.lib;
            doCheck = false;
          };
          musl64 =
            let
              pkgs' = pkgs.pkgsCross.musl64;
            in
            pkgs'.lib.callPackageWith pkgs' ./nix {
              static = true;
              doCheck = false;
              nix-filter = nix-filter.lib;
            };

        };
        defaultPackage = packages.native.piaf;
        devShell = pkgs.callPackage ./shell.nix { inherit packages; };
        gh-actions = pkgs.callPackage ./nix/gh-actions.nix { };
      });
}
