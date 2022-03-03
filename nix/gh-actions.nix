{ lib }:

let
  commonSteps = [
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
        "signingKey" = "\${{ secrets.CACHIX_SIGNING_KEY }}";
      };
    }

  ];

  job = { steps, ... }@attrs: attrs // {
    "strategy" = {
      "fail-fast" = false;
      "matrix" = {
        "ocamlVersion" = [
          "4_12"
          "4_13"
        ];
      };
    };
    "steps" = commonSteps ++ steps;

  };
in

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
    "macOS" = job {
      "runs-on" = "macos-latest";
      steps = [
        {
          "name" = "Run nix-build";
          "run" = "nix-build ./nix/ci/test.nix -A native --argstr ocamlVersion \${{ matrix.ocamlVersion }}";
        }
      ];
    };
    "linux" = job {
      "runs-on" = "ubuntu-latest";
      "steps" = [
        {
          "name" = "Run nix-build";
          "run" = "nix-build ./nix/ci/test.nix -A native -A musl64 --argstr ocamlVersion \${{ matrix.ocamlVersion }}";
        }
      ];
    };
  };
}
