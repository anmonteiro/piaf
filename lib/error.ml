type kind =
  [ `Connection
  | `Stream
  ]

type t =
  [ `Exn of exn
  | `Invalid_response_body_length of H2.Status.t * Headers.t
  | `Malformed_response of string
  | `Protocol_error of H2.Error_code.t * string
  | `Connect_error of string
  | `Msg of string
  ]

let to_string = function
  | `Exn exn ->
    Printexc.to_string exn
  | `Invalid_response_body_length (status, headers) ->
    Format.asprintf
      "Invalid Response body length. Status: %a, Headers: %a"
      H2.Status.pp_hum
      status
      Headers.pp_hum
      headers
  | `Malformed_response msg ->
    Format.asprintf "Malformed Response: %s" msg
  | `Protocol_error (code, msg) ->
    Format.asprintf
      "Protocol Error (%a)%s%s"
      H2.Error_code.pp_hum
      code
      (if msg = "" then "" else ": ")
      msg
  | `Connect_error msg ->
    Format.asprintf "Connect Error: %s" msg
  | `Msg string ->
    string

let pp_hum formatter t = Format.fprintf formatter "%s" (to_string t)
