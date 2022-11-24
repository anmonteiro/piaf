# piaf

Piaf is a client library for the HTTP/1.X and HTTP/2 protocols written entirely
in OCaml.

## Installation

Piaf is released to OPAM.

You can depend on it via [esy](esy) or by running `opam install piaf`.

_Note_: make sure to mirror Piaf's own resolutions located in the [opam
file](./piaf.opam).

[esy]: https://esy.sh

# Usage & Examples

TODO, read the [mli](./lib/piaf.mli) file for now.

### Examples

```ml
open Piaf

let get_sync env ~sw url =
  print_endline "Sending request...";
  match Client.Oneshot.get ~sw env (Uri.of_string url) with
  | Ok response ->
    if Status.is_successful response.status
    then Body.to_string response.body
    else
      let message = Status.to_string response.status in
      Error (`Msg message)
  | Error e -> failwith (Error.to_string e)

let () =
  Eio_main.run (fun env ->
      Eio.Switch.run (fun sw ->
          match get_sync env ~sw "https://example.com" with
          | Ok body -> print_endline body
          | Error error ->
            let message = Error.to_string error in
            prerr_endline ("Error: " ^ message)))
```

There's a more substantive example of using Piaf's API in
[bin/carl.ml](./bin/carl.ml), an implementation of a subset of curl, in caml.

## Development

There's two ways to get a development environemnt up and running. If you have (or don't mind getting) [`nix`](https://nixos.org/nix/manual/) installed, the repository includes scripts to set up a sandbox. Otherwise you can use [`opam`](https://opam.ocaml.org/) to install the necessary dependencies globally.

### Option 1) Setting up the sandbox

Assuming [`nix` has been installed](https://nix.dev/tutorials/install-nix.html) and set up, run `nix develop -c $SHELL` in the repository root. Once it's done building, you should have the development environment set up!

### Option 2) Setting up opam

For this approach you'll need to [install `opam`](https://opam.ocaml.org/doc/Install.html)  and set it up with a switch using `ocaml` >= 4.08. Once that's done, run `opam pin . --deps-only` to install the dependencies.

Note that this installs the dependencies globally, and that the development environment is dependent on the switch used.

### Building

Run `dune build` to build, `dune build --watch` to run a watcher daemon that will build incrementally.

### Running examples

Run `dune exec examples/docs/readme.exe` to run the simple example above.

Run `dune exec bin/carl.exe` to run `carl`, the `curl`-like example.

## License & Copyright

Copyright (c) 2019 António Nuno Monteiro

piaf is distributed under the 3-Clause BSD License, see [LICENSE](./LICENSE).

The `vendor/multipart_form` directory contains a fork of
[`multipart_form`](https://github.com/dinosaure/multipart_form) which is
licensed under the MIT License.
[`multipart_form.LICENSE`](./multipart_form.LICENSE) reproduces the original
license text.
