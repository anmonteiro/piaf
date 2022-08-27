{ callPackage
, ocamlPackages
, nix-filter
, pkgsCross
, doCheck ? false
}:

{
  native = callPackage ./generic.nix {
    inherit doCheck nix-filter;
  };

  musl64 = pkgsCross.musl64.callPackage
    ({ pkgs }:
      pkgs.callPackage ./generic.nix {
        static = true;
        inherit doCheck nix-filter;
        inherit (pkgs) ocamlPackages;
      })
    { };
}
