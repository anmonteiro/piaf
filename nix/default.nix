{ callPackage
, ocamlPackages
, pkgsCross
, doCheck ? false
}:

{
  native = callPackage ./generic.nix {
    inherit doCheck;
  };

  musl64 =
    let
      pkgs' = pkgsCross.musl64;
    in
    pkgs'.callPackage ./generic.nix {
      static = true;
      inherit doCheck;
    };
}
