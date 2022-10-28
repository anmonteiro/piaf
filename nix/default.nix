{ pkgs ? import ./sources.nix { }, nix-filter, doCheck ? false }:

{
  native = pkgs.callPackage ./generic.nix {
    inherit doCheck nix-filter;
  };

  musl64 =
    let
      pkgs' = pkgs.pkgsCross.musl64;
    in
    pkgs'.callPackage ./generic.nix {
      static = true;
      inherit doCheck nix-filter;
      ocamlPackages = pkgs'.ocamlPackages;
    };
}
