{
  description = "Piaf Nix Flake";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:anmonteiro/nix-overlays";
  inputs.nix-filter.url = "github:numtide/nix-filter";
  # inputs.nixpkgs.url = "/home/anmonteiro/projects/nix-overlays";

  outputs = { self, nixpkgs, flake-utils, nix-filter }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages."${system}";
      in
      rec {
        packages = pkgs.callPackage ./nix { inherit pkgs nix-filter; };
        defaultPackage = packages.native.piaf;
        devShell = import ./shell.nix { inherit pkgs nix-filter; };
      });
}
