{ callPackage
, ocamlPackages
, pkgsCross
, doCheck ? false
}:

{
  native = callPackage ./generic.nix {
    inherit doCheck;
  };

  musl64 = pkgsCross.musl64.callPackage
    ({ callPackage, ocamlPackages }:
      callPackage ./generic.nix
        {
          static = true;
          inherit doCheck;
        })
    { };
}
