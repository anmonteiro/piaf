type disposition_type =
  [ `Inline | `Attachment | `Ietf_token of string | `X_token of string ]

type t = {
  ty : disposition_type;
  filename : string option;
  creation : date option;
  modification : date option;
  read : date option;
  size : int option;
  parameters : (string * value) list;
}

and value = String of string | Token of string

and date = unit

let v ?filename ?(kind = `Ietf_token "form-data") ?size name =
  {
    ty = kind;
    filename;
    creation = None;
    modification = None;
    read = None;
    size;
    parameters = [ ("name", String name) ];
  }

let pp_disposition_type ppf = function
  | `Inline -> Fmt.string ppf "inline"
  | `Attachment -> Fmt.string ppf "attachment"
  | `Ietf_token v -> Fmt.pf ppf "<ietf:%s>" v
  | `X_token v -> Fmt.pf ppf "X-%s" v

let pp_value ppf = function
  | String v -> Fmt.pf ppf "%S" v
  | Token v -> Fmt.string ppf v

let pp ppf t =
  Fmt.pf ppf
    "{ @[<hov>type= %a;@ filename= %a;@ creation= %a;@ modification= %a;@ \
     read= %a;@ size= %a;@ parameters= @[<hov>%a@];@] }"
    pp_disposition_type t.ty
    Fmt.(option string)
    t.filename
    Fmt.(option (unit "#date"))
    t.creation
    Fmt.(option (unit "#date"))
    t.modification
    Fmt.(option (unit "#date"))
    t.read
    Fmt.(option int)
    t.size
    Fmt.(
      Dump.iter_bindings
        (fun f -> List.iter (fun (k, v) -> f k v))
        (always "parameters") string pp_value)
    t.parameters

let name t =
  match List.assoc_opt "name" t.parameters with
  | Some (String v | Token v) -> Some v
  | None -> None

let filename { filename; _ } = filename

let size { size; _ } = size

let disposition_type { ty; _ } = ty

let of_escaped_character = function
  | '\x61' -> '\x07' (* "\a" *)
  | '\x62' -> '\x08' (* "\b" *)
  | '\x74' -> '\x09' (* "\t" *)
  | '\x6E' -> '\x0A' (* "\n" *)
  | '\x76' -> '\x0B' (* "\v" *)
  | '\x66' -> '\x0C' (* "\f" *)
  | '\x72' -> '\x0D' (* "\r" *)
  | c -> c

let is_obs_no_ws_ctl = function
  | '\001' .. '\008' | '\011' | '\012' | '\014' .. '\031' | '\127' -> true
  | _ -> false

let is_qtext = function
  | '\033' | '\035' .. '\091' | '\093' .. '\126' -> true
  | c -> is_obs_no_ws_ctl c

let is_wsp = function ' ' | '\t' -> true | _ -> false

module Rfc2045 = struct
  open Angstrom

  let _3 x y z = (x, y, z)

  let _4 a b c d = (a, b, c, d)

  let ( .![]<- ) = Bytes.set

  let utf_8_tail = satisfy @@ function '\x80' .. '\xbf' -> true | _ -> false

  let utf_8_0 =
    satisfy (function '\xc2' .. '\xdf' -> true | _ -> false) >>= fun b0 ->
    utf_8_tail >>= fun b1 ->
    let res = Bytes.create 2 in
    res.![0] <- b0 ;
    res.![1] <- b1 ;
    return (Bytes.unsafe_to_string res)

  let utf_8_1 =
    lift3 _3 (char '\xe0')
      (satisfy @@ function '\xa0' .. '\xbf' -> true | _ -> false)
      utf_8_tail
    <|> lift3 _3
          (satisfy @@ function '\xe1' .. '\xec' -> true | _ -> false)
          utf_8_tail utf_8_tail
    <|> lift3 _3 (char '\xed')
          (satisfy @@ function '\x80' .. '\x9f' -> true | _ -> false)
          utf_8_tail
    <|> lift3 _3
          (satisfy @@ function '\xee' .. '\xef' -> true | _ -> false)
          utf_8_tail utf_8_tail

  let utf_8_1 =
    utf_8_1 >>= fun (b0, b1, b2) ->
    let res = Bytes.create 3 in
    res.![0] <- b0 ;
    res.![1] <- b1 ;
    res.![2] <- b2 ;
    return (Bytes.unsafe_to_string res)

  let utf_8_2 =
    lift4 _4 (char '\xf0')
      (satisfy @@ function '\x90' .. '\xbf' -> true | _ -> false)
      utf_8_tail utf_8_tail
    <|> lift4 _4
          (satisfy @@ function '\xf1' .. '\xf3' -> true | _ -> false)
          utf_8_tail utf_8_tail utf_8_tail
    <|> lift4 _4 (char '\xf4')
          (satisfy @@ function '\x80' .. '\x8f' -> true | _ -> false)
          utf_8_tail utf_8_tail

  let utf_8_2 =
    utf_8_2 >>= fun (b0, b1, b2, b3) ->
    let res = Bytes.create 4 in
    res.![0] <- b0 ;
    res.![1] <- b1 ;
    res.![2] <- b2 ;
    res.![3] <- b3 ;
    return (Bytes.unsafe_to_string res)

  let utf_8_and is =
    satisfy is >>| String.make 1 <|> utf_8_0 <|> utf_8_1 <|> utf_8_2

  let quoted_pair =
    char '\\' *> any_char >>| of_escaped_character >>| String.make 1

  let quoted_string =
    char '"' *> many (quoted_pair <|> utf_8_and is_qtext)
    <* char '"'
    >>| String.concat ""
end

module Decoder = struct
  type parameter =
    | Filename of value
    | Creation of unit
    | Modification of unit
    | Read of unit
    | Size of int
    | Parameter of (string * value)

  open Angstrom

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

  let invalid_token token = Fmt.kstrf fail "invalid token: %s" token

  let nothing_to_do = Fmt.kstrf fail "nothing to do"

  (* / *)

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

  let is_digit = function '0' .. '9' -> true | _ -> false

  (* From RFC 2045

          attribute := token
                       ; Matching of attributes
                       ; is ALWAYS case-insensitive.
  *)
  let attribute = token >>| String.lowercase_ascii

  (* From RFC 2045

          ietf-token := <An extension token defined by a
                            standards-track RFC and registered
                            with IANA.>
          iana-token := <A publicly-defined extension token. Tokens
                            of this form must be registered with IANA
                            as specified in RFC 2048.>

     XXX(dinosaure): we don't check at this time if IETF/IANA token exists.
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

  (* From RFC 2045

          value := token / quoted-string
  *)
  let value =
    Rfc2045.quoted_string
    >>| (fun v -> String v)
    <|> (token >>| fun v -> Token v)

  let of_string s a =
    match parse_string ~consume:All a s with Ok v -> Some v | Error _ -> None

  let disposition_type =
    token >>= fun s ->
    match String.lowercase_ascii s with
    | "inline" -> return `Inline
    | "attachment" -> return `Attachment
    | _ ->
    match of_string s extension_token with
    | Some v -> return v
    | None -> invalid_token s

  (* From RFC 2045

          parameter := attribute "=" value
  *)
  let parameter =
    attribute >>= fun attribute ->
    skip_while is_wsp *> char '=' *> skip_while is_wsp *> value >>| fun value ->
    (attribute, value)

  let quoted_date_time = value >>| fun _ -> ()

  let parm parm value =
    string parm *> skip_while is_wsp *> char '=' *> skip_while is_wsp *> value

  let filename_parm = parm "filename" value

  let creation_date_parm = parm "creation-date" quoted_date_time

  let modification_date_parm = parm "modification-date" quoted_date_time

  let read_date_parm = parm "read-date" quoted_date_time

  let size_parm = parm "read-date" (take_while1 is_digit >>| int_of_string)

  let disposition_parm =
    filename_parm
    >>| (fun v -> Filename v)
    <|> (creation_date_parm >>| fun v -> Creation v)
    <|> (modification_date_parm >>| fun v -> Modification v)
    <|> (read_date_parm >>| fun v -> Read v)
    <|> (size_parm >>| fun v -> Size v)
    <|> (parameter >>| fun v -> Parameter v)

  let disposition =
    skip_while is_wsp *> disposition_type <* skip_while is_wsp >>= fun ty ->
    many (skip_while is_wsp *> char ';' *> skip_while is_wsp *> disposition_parm)
    >>= fun parameters ->
    let filename = ref None in
    let creation = ref None in
    let modification = ref None in
    let read = ref None in
    let size = ref None in
    let parameters =
      List.fold_left
        (fun a -> function
          | Filename (String v) | Filename (Token v) ->
              filename := Some v ;
              a
          | Creation v ->
              creation := Some v ;
              a
          | Modification v ->
              modification := Some v ;
              a
          | Read v ->
              read := Some v ;
              a
          | Size v ->
              size := Some v ;
              a
          | Parameter v -> v :: a)
        [] parameters in
    return
      {
        ty;
        filename = !filename;
        creation = !creation;
        modification = !modification;
        read = !read;
        size = !size;
        parameters;
      }
end

module Encoder = struct
  open Prettym

  let disposition_type ppf = function
    | `Attachment -> eval ppf [ string $ "attachment" ]
    | `Inline -> eval ppf [ string $ "inline" ]
    | `Ietf_token v -> eval ppf [ !!string ] v
    | `X_token v -> eval ppf [ string $ "X-"; !!string ] v

  let token ppf str = eval ppf [ !!string ] str

  (* TODO(dinosaure): unsafe token. *)

  let value ppf = function
    | Token v -> token ppf v
    | String v -> eval ppf [ char $ '"'; !!string; char $ '"' ] v

  type 'a param =
    | Filename : string param
    | Creation : date param
    | Modification : date param
    | Read : date param
    | Size : int param
    | Parameter : (string * value) param

  type v = V : ('a param * 'a option) -> v

  let disposition_parm : v Prettym.t =
   fun ppf (V v) ->
    match v with
    | Filename, Some v ->
        eval ppf
          [ string $ "filename"; cut; char $ '='; !!token; char $ ';'; fws ]
          v
    | Filename, None -> ppf
    | Creation, _ -> ppf
    | Modification, _ -> ppf
    | Read, _ -> ppf
    | Size, Some v ->
        eval ppf
          [ string $ "size"; cut; char $ '='; !!string; char $ ';'; fws ]
          (string_of_int v)
    | Size, None -> ppf
    | Parameter, Some (k, v) ->
        eval ppf [ !!string; cut; char $ '='; !!value; char $ ';'; fws ] k v
    | Parameter, None -> ppf

  let disposition ppf v =
    let sep ppf () = eval ppf [ cut ] in
    eval ppf
      [
        !!disposition_type;
        cut;
        char $ ';';
        fws;
        !!(list ~sep:(sep, ()) disposition_parm);
      ]
      v.ty
      ([
         V (Filename, v.filename);
         V (Creation, v.creation);
         V (Modification, v.modification);
         V (Read, v.read);
         V (Size, v.size);
       ]
      @ List.map (fun (k, v) -> V (Parameter, Some (k, v))) v.parameters)
end
