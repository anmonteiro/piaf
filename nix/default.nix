{ pkgs ? import ./sources.nix {}, doCheck ? false }:

{
  native = pkgs.callPackage ./generic.nix {
    inherit doCheck;
  };

  musl64 = pkgs.pkgsCross.musl64.pkgsStatic.callPackage ./generic.nix {
    static = true;
    inherit doCheck;
  };
}
