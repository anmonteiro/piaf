{ pkgs ? import ./sources.nix {}, doCheck ? false }:

{
  native = pkgs.callPackage ./generic.nix {
    inherit doCheck;
  };

  musl64 =
    let
      pkgs' = pkgs.pkgsCross.musl64;
    in
    pkgs'.callPackage ./generic.nix {
      static = true;
      inherit doCheck;
      ocamlPackages = pkgs'.ocamlPackages;
    };
}
