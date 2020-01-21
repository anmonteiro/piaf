let
  sources = import ./nix/sources.nix {};
  inherit (sources) pkgs;
in
  with pkgs;

  mkShell {
    inputsFrom = [ (import ./nix {}).native.carl ];
    buildInputs = with ocamlPackages; [ merlin ocamlformat utop ];
  }
