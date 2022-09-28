{
  description = "Piaf Nix Flake";

  inputs.nix-filter.url = "github:numtide/nix-filter";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.inputs.flake-utils.follows = "flake-utils";
  inputs.nixpkgs.url = "github:nix-ocaml/nix-overlays?rev=12fe71dd3e296c2111a251d4afec3f05909b9c65";

  outputs = { self, nixpkgs, flake-utils, nix-filter }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages."${system}".extend (self: super: {
          ocamlPackages = super.ocaml-ng.ocamlPackages_5_00;
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
