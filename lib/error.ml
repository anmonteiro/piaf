(*----------------------------------------------------------------------------
 * Copyright (c) 2020-2023 António Nuno Monteiro
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

type kind =
  [ `Connection
  | `Stream
  ]

type common =
  [ `Exn of exn
  | `Protocol_error of H2.Error_code.t * string
  | `TLS_error of string
  | `Upgrade_not_supported
  | `Msg of string
  ]

type client =
  [ `Invalid_response_body_length of H2.Status.t * Headers.t
  | `Malformed_response of string
  | `Connect_error of string
  | common
  ]

type server =
  [ `Bad_gateway
  | `Bad_request
  | `Internal_server_error
  | common
  ]

type t =
  [ common
  | client
  | server
  ]

let to_string = function
  | `Exn exn -> Printexc.to_string exn
  | `Invalid_response_body_length (status, headers) ->
    Format.asprintf
      "Invalid Response body length. Status: %a, Headers: %a"
      H2.Status.pp_hum
      status
      Headers.pp_hum
      headers
  | `Malformed_response msg -> Format.asprintf "Malformed Response: %s" msg
  | `Protocol_error (code, msg) ->
    Format.asprintf
      "Protocol Error (%a)%s%s"
      H2.Error_code.pp_hum
      code
      (if msg = "" then "" else ": ")
      msg
  | `TLS_error msg -> Format.asprintf "SSL Error: %s" msg
  | `Connect_error msg -> Format.asprintf "Connection Error: %s" msg
  | `Bad_gateway -> "Bad Gateway"
  | `Bad_request -> "Bad Request"
  | `Internal_server_error -> "Internal Server Error"
  | `Upgrade_not_supported -> "Upgrades not supported in this HTTP Version"
  | `Msg string -> string

let pp_hum formatter t = Format.fprintf formatter "%s" (to_string t)
