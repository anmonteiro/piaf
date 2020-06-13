# piaf

Piaf is a client library for the HTTP/1.X and HTTP/2 protocols written entirely
in OCaml.

## Installation

Piaf is currently unreleased.

You can depend on it via [esy](esy) resolutions or `opam pin`

_Note_: make sure to mirror Piaf's own resolutions located in the [opam
file](./piaf.opam).

[esy]: https://esy.sh

# Usage & Examples

TODO, read the [mli](./lib/piaf.mli) file for now.

### Examples

There's an example of using Piaf's API in [bin/carl.ml](./bin/carl.ml), an
implementation of a subset of curl, in caml.

## Development

There's two ways to get a development environemnt up and running. If you have (or don't mind getting) [`nix`](https://nixos.org/nix/manual/) installed, the repository includes scripts to set up a sandbox. Otherwise you can use [`opam`](https://opam.ocaml.org/) to install the necessary dependencies globally.

### Option 1) Setting up the sandbox

Assuming [`nix` has been installed](https://nix.dev/tutorials/install-nix.html) and set up, run `nix-shell --pure` in the repository root. Once it's done building, you should have the development environment set up!

### Option 2) Setting up opam

Fot this approach you'll need to [install `opam`](https://opam.ocaml.org/doc/Install.html)  and set it up with a switch using `ocaml` >= 4.06. Once that's done, run `opam pin . --deps-only` to install the dependencies.

Note that this installs the dependencies globally, and that the development environment is dependent on the switch used.

### Building

Run `dune build` to build, `dune build --watch` to run a watcher dameon that will build incremenetally.

### Running examples

Run `dune exec examples/docs/readme.exe` to run the simple example above.

Run `dune exec bin/carl.exe` to run the `carl`, the `curl`-like example.

## License & Copyright

Copyright (c) 2019 António Nuno Monteiro

piaf is distributed under the 3-Clause BSD License, see [LICENSE](./LICENSE).

