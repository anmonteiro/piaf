{ lib }:

lib.generators.toYAML { }
{
  name = "Build";
  "on" = {
    "pull_request" = null;
    "push" = {
      "branches" = [
        "master"
      ];
    };
  };
  "jobs" = {
    "macOS" = {
      "runs-on" = "macos-latest";
      "strategy" = {
        "fail-fast" = false;
        "matrix" = {
          "ocamlVersion" = [
            "4_12"
            "4_13"
          ];
        };
      };
      "steps" = [
        {
          "uses" = "actions/checkout@v2";
          "with" = {
            "submodules" = "recursive";
          };
        }
        {
          "uses" = "cachix/install-nix-action@v14.1";
        }
        {
          "uses" = "cachix/cachix-action@v10";
          "with" = {
            "name" = "anmonteiro";
            "signingKey" = "''$''{{ secrets.CACHIX_SIGNING_KEY }}";
          };
        }
        {
          "name" = "Run nix-build";
          "run" = "nix-build ./nix/ci/test.nix -A native --argstr ocamlVersion ''$''{{ matrix.ocamlVersion }}";
        }
      ];
    };
    "linux" = {
      "runs-on" = "ubuntu-latest";
      "strategy" = {
        "fail-fast" = false;
        "matrix" = {
          "ocamlVersion" = [
            "4_11"
            "4_12"
            "4_13"
          ];
        };
      };
      "steps" = [
        {
          "uses" = "actions/checkout@v2";
          "with" = {
            "submodules" = "recursive";
          };
        }
        {
          "uses" = "cachix/install-nix-action@v14.1";
        }
        {
          "uses" = "cachix/cachix-action@v10";
          "with" = {
            "name" = "anmonteiro";
            "signingKey" = "''$''{{ secrets.CACHIX_SIGNING_KEY }}";
          };
        }
        {
          "name" = "Run nix-build";
          "run" = "nix-build ./nix/ci/test.nix -A native -A musl64 --argstr ocamlVersion ''$''{{ matrix.ocamlVersion }}";
        }
      ];
    };
  };
}
