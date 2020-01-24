let
  pkgs = import ./nix/sources.nix {};
in
  with pkgs;

  mkShell {
    inputsFrom = [ (import ./nix {}).native.carl ];
    buildInputs = with ocamlPackages; [ merlin ocamlformat utop ];
  }
