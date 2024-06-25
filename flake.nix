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
          ocamlPackages = super.ocaml-ng.ocamlPackages_5_2;
        });
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
      in
      {
        packages = packages // { default = packages.native.piaf; };
        devShells = {
          default = pkgs.callPackage ./nix/shell.nix { inherit packages; };
        };
        gh-actions = pkgs.callPackage ./nix/gh-actions.nix { };
      });
}
