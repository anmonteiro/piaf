(*----------------------------------------------------------------------------
 * Copyright (c) 2019-2020, António Nuno Monteiro
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *---------------------------------------------------------------------------*)

type t =
  | HTTP
  | HTTPS

let of_uri uri =
  match Uri.scheme uri with
  | None | Some "http" -> Ok HTTP
  | Some "https" -> Ok HTTPS
  (* We don't support anything else *)
  | Some other -> Error (`Msg (Format.asprintf "Unsupported scheme: %s" other))

let to_string = function HTTP -> "http" | HTTPS -> "https"
let pp_hum formatter scheme = Format.fprintf formatter "%s" (to_string scheme)

module Runtime = struct
  type t =
    | HTTP : Gluten_lwt_unix.Client.t -> t
    | HTTPS : Gluten_lwt_unix.Client.SSL.t -> t

  module type SCHEME = sig
    type runtime

    val make : runtime -> t
  end

  module HTTP = struct
    type runtime = Gluten_lwt_unix.Client.t

    let make x = HTTP x
  end

  module HTTPS = struct
    type runtime = Gluten_lwt_unix.Client.SSL.t

    let make x = HTTPS x
  end
end
