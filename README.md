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

```ml
open Piaf

let get_sync url =
  let open Lwt_result.Syntax in
  
  Lwt_main.run begin
    print_endline("Sending request...");
    
    let* response = Client.Oneshot.get (Uri.of_string url) in
    
    if (Status.is_successful response.status) then
      Body.to_string response.body
    else
      let message = Status.to_string response.status in
      Lwt.return (Error (`Msg message))
  end

let () =
  match get_sync "https://example.com" with
  | Ok body -> print_endline body
  | Error error ->
    let message = Error.to_string error in
    prerr_endline ("Error: " ^ message)
```

There's a more substantive example of using Piaf's API in
[bin/carl.ml](./bin/carl.ml), an implementation of a subset of curl, in caml.

## License & Copyright

Copyright (c) 2019 António Nuno Monteiro

piaf is distributed under the 3-Clause BSD License, see [LICENSE](./LICENSE).

