open Stdlib
open Angstrom

(* From RFC 2046

   bcharsnospace := DIGIT / ALPHA / "'" / "(" / ")" / "+" / "_" / "," / "-" /
   "." / "/" / ":" / "=" / "?" *)
let is_bcharsnospace = function
  | '\'' | '(' | ')' | '+' | '_' | ',' | '-' | '.' | '/' | ':' | '=' | '?' ->
      true
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | '0' .. '9' -> true
  | _ -> false

(* From RFC 2046

     bchars := bcharsnospace / " "
*)
let is_bchars = function ' ' -> true | c -> is_bcharsnospace c

(* From RFC 2046

     dash-boundary := "--" boundary
                      ; boundary taken from the value of
                      ; boundary parameter of the
                      ; Content-Type field.
*)
let make_dash_boundary boundary = "--" ^ boundary

let dash_boundary boundary = string (make_dash_boundary boundary)

let make_delimiter boundary = "\r\n" ^ make_dash_boundary boundary

let make_close_delimiter boundary = make_delimiter boundary ^ "--"

let close_delimiter boundary = string (make_close_delimiter boundary)

(* NOTE: this parser terminate at the boundary, however it does not consume it. *)
let discard_all_to_dash_boundary boundary =
  let check_boundary =
    let dash_boundary = make_dash_boundary boundary in
    let expected_len = String.length dash_boundary in
    Unsafe.peek expected_len (fun ba ~off ~len ->
        let raw = Bigstringaf.substring ba ~off ~len in
        String.equal raw dash_boundary) in
  fix @@ fun m ->
  skip_while (( <> ) '-') *> peek_char >>= function
  | Some '-' -> (
      check_boundary >>= function true -> return () | false -> advance 1 *> m)
  | Some _ -> advance 1 *> m (* impossible case? *)
  | None -> return ()

(* From RFC 2046

     transport-padding := *LWSP-char
                          ; Composers MUST NOT generate
                          ; non-zero length transport
                          ; padding, but receivers MUST
                          ; be able to handle padding
                          ; added by message transports.
*)
let transport_padding =
  skip_while (function '\x09' | '\x20' -> true | _ -> false)

let discard_all_to_delimiter boundary =
  let check_delimiter =
    let delimiter = make_delimiter boundary in
    let expected_len = String.length delimiter in
    Unsafe.peek expected_len (fun ba ~off ~len ->
        let raw = Bigstringaf.substring ba ~off ~len in
        String.equal raw delimiter) in
  fix @@ fun m ->
  skip_while (( <> ) '\r') *> peek_char >>= function
  | Some '\r' -> (
      check_delimiter >>= function true -> return () | false -> advance 1 *> m)
  | Some _ -> advance 1 *> m (* impossible case? *)
  | None -> return ()

let nothing_to_do = Fmt.kstr fail "nothing to do"

let crlf = char '\r' *> char '\n'

let body_part body =
  Header.Decoder.header >>= fun fields ->
  (crlf *> return `CRLF <|> return `Nothing <* commit >>= function
   | `CRLF -> body fields >>| Option.some
   | `Nothing -> return None)
  >>| fun body -> (fields, body)

let encapsulation boundary body =
  string (make_delimiter boundary)
  *> transport_padding
  *> crlf
  *> commit
  *> body_part body

(* From RFC 2046:

   preamble := discard-text discard-text := *( *text CRLF) ; May be ignored or
   discarded.

   XXX(dinosaure): this parser consume the last CRLF which is NOT included in
   the ABNF. *)
let preambule boundary = discard_all_to_dash_boundary boundary

let epilogue parent =
  match parent with
  | Some boundary -> discard_all_to_delimiter boundary
  | None -> skip_while (fun _ -> true)

let multipart_body ?parent boundary body =
  option () (preambule boundary) (* see [preambule]. *)
  *> dash_boundary boundary
  *> transport_padding
  *> crlf
  *> commit
  *> body_part body
  >>= fun x ->
  many (encapsulation boundary body) >>= fun r ->
  (commit
   *> close_delimiter boundary
   *> transport_padding
   *> option () (epilogue parent)
  <|> return ())
  *> return (x :: r)
