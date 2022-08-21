{
  description = "Piaf Nix Flake";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.inputs.flake-utils.follows = "flake-utils";
  inputs.nixpkgs.url = "github:anmonteiro/nix-overlays";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages."${system}".extend (self: super: {
          ocamlPackages = super.ocaml-ng.ocamlPackages_5_00;
        });
      in
      rec {
        packages = (pkgs.callPackage ./nix { }) // { };
        defaultPackage = packages.native.piaf;
        devShell = pkgs.callPackage ./shell.nix { };
        gh-actions = pkgs.callPackage ./nix/gh-actions.nix { };
      });
}
