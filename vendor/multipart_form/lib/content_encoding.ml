type t =
  [ `Bit7
  | `Bit8
  | `Binary
  | `Quoted_printable
  | `Base64
  | `Ietf_token of string
  | `X_token of string ]

let pp ppf = function
  | `Bit7 -> Fmt.string ppf "7bit"
  | `Bit8 -> Fmt.string ppf "8bit"
  | `Binary -> Fmt.string ppf "binary"
  | `Quoted_printable -> Fmt.string ppf "quoted-printable"
  | `Base64 -> Fmt.string ppf "base64"
  | `Ietf_token token -> Fmt.pf ppf "ietf:%s" token
  | `X_token token -> Fmt.pf ppf "x:%s" token

let default = `Bit7

let bit8 = `Bit8

let bit7 = `Bit7

let binary = `Binary

let quoted_printable = `Quoted_printable

let base64 = `Base64

let of_string = function
  | "7bit" -> Ok `Bit7
  | "8bit" -> Ok `Bit8
  | "binary" -> Ok `Binary
  | "quoted-printable" -> Ok `Quoted_printable
  | "base64" -> Ok `Base64
  | x -> Rresult.R.error_msgf "Invalid MIME encoding: %s" x

(* TODO:
   - let the user to craft an extension token.
   - check IETF database *)

let equal a b =
  match (a, b) with
  | `Bit7, `Bit7 -> true
  | `Bit8, `Bit8 -> true
  | `Binary, `Binary -> true
  | `Quoted_printable, `Quoted_printable -> true
  | `Base64, `Base64 -> true
  | `Ietf_token a, `Ietf_token b ->
      String.(equal (lowercase_ascii a) (lowercase_ascii b))
  | `X_token a, `X_token b ->
      String.(equal (lowercase_ascii a) (lowercase_ascii b))
  | _, _ -> false

module Decoder = struct
  open Angstrom

  let invalid_token token = Fmt.kstrf fail "invalid token: %s" token

  let of_string s a =
    match parse_string ~consume:Consume.All a s with
    | Ok v -> Some v
    | Error _ -> None

  (* From RFC 2045

          tspecials :=  "(" / ")" / "<" / ">" / "@" /
                        "," / ";" / ":" / "\" / <">
                        "/" / "[" / "]" / "?" / "="
                        ; Must be in quoted-string,
                        ; to use within parameter values

        Note that the definition of "tspecials" is the same as the RFC 822
        definition of "specials" with the addition of the three characters
        "/", "?", and "=", and the removal of ".".
  *)
  let is_tspecials = function
    | '(' | ')' | '<' | '>' | '@' | ',' | ';' | ':' | '\\' | '"' | '/' | '['
    | ']' | '?' | '=' ->
        true
    | _ -> false

  let is_ctl = function '\000' .. '\031' | '\127' -> true | _ -> false

  let is_space = ( = ) ' '

  (* From RFC 2045

          token := 1*<any (US-ASCII) CHAR except SPACE, CTLs,
                      or tspecials>
  *)
  let is_ascii = function '\000' .. '\127' -> true | _ -> false

  let is_token c =
    is_ascii c && (not (is_tspecials c)) && (not (is_ctl c)) && not (is_space c)

  let token = take_while1 is_token

  (* From RFC 2045

          ietf-token := <An extension token defined by a
                            standards-track RFC and registered
                            with IANA.>

     XXX(dinosaure): we don't check at this time if IETF token exists.
  *)
  let ietf_token = token

  (* From RFC 2045

          x-token := <The two characters "X-" or "x-" followed, with
                         no intervening white space, by any token>
  *)
  let x_token =
    satisfy (function 'x' | 'X' -> true | _ -> false) *> char '-' *> token

  (* From RFC 2045

          extension-token := ietf-token / x-token
  *)
  let extension_token =
    peek_char >>= function
    | Some 'X' | Some 'x' -> x_token >>| fun v -> `X_token v
    | _ -> ietf_token >>| fun v -> `Ietf_token v

  let is_wsp = function ' ' | '\t' -> true | _ -> false

  (* From RFC 2045

          mechanism := "7bit" / "8bit" / "binary" /
                       "quoted-printable" / "base64" /
                       ietf-token / x-token

        These values are not case sensitive -- Base64 and BASE64 and bAsE64
        are all equivalent.  An encoding type of 7BIT requires that the body
        is already in a 7bit mail-ready representation.  This is the default
        value -- that is, "Content-Transfer-Encoding: 7BIT" is assumed if the
        Content-Transfer-Encoding header field is not present.
  *)
  let mechanism =
    skip_while is_wsp *> token <* skip_while is_wsp >>= fun s ->
    (* XXX(dinosaure): lowercase_*ascii* is fine, not utf8 in this part. *)
    match String.lowercase_ascii s with
    | "7bit" -> return `Bit7
    | "8bit" -> return `Bit8
    | "binary" -> return `Binary
    | "quoted-printable" -> return `Quoted_printable
    | "base64" -> return `Base64
    | _ ->
    match of_string s extension_token with
    | Some v -> return v
    | None -> invalid_token s
end

module Encoder = struct
  open Prettym

  let mechanism ppf = function
    | `Bit7 -> string ppf "7bit"
    | `Bit8 -> string ppf "8bit"
    | `Binary -> string ppf "binary"
    | `Quoted_printable -> string ppf "quoted-printable"
    | `Base64 -> string ppf "base64"
    | `Ietf_token x -> string ppf x
    | `X_token x -> eval ppf [ string $ "X-"; !!string ] x
end
