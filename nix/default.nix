{ callPackage
, ocamlPackages
, pkgsCross
, doCheck ? false
}:

{
  native = callPackage ./generic.nix {
    inherit doCheck;
  };

  musl64 = pkgsCross.musl64.callPackage ./generic.nix {
    static = true;
    inherit doCheck;
  };
}
