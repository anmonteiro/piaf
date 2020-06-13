# piaf

Piaf is a client library for the HTTP/1.X and HTTP/2 protocols written entirely
in OCaml.

## Installation

Piaf is currently unreleased.

You can depend on it via [esy](esy) resolutions or `opam pin`

_Note_: make sure to mirror Piaf's own resolutions located in the [opam
file](./piaf.opam).

[esy]: https://esy.sh

[reason-native-web](https://github.com/reason-native-web) has published an NPM package that can easily be consumed with esy, although note that this is strictly not supported and might be severely out-of-date:

```
esy add @reason-native-web/piaf
```

# Usage & Examples

TODO, read the [mli](./lib/piaf.mli) file for now.

### Examples

There's an example of using Piaf's API in [bin/carl.ml](./bin/carl.ml), an
implementation of a subset of curl, in caml.

## License & Copyright

Copyright (c) 2019 António Nuno Monteiro

piaf is distributed under the 3-Clause BSD License, see [LICENSE](./LICENSE).

