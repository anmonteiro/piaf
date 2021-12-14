exception Invalid_token

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
  | '(' | ')' | '<' | '>' | '@' | ',' | ';' | ':' | '\\' | '"' | '/' | '[' | ']'
  | '?' | '=' ->
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

let is_obs_no_ws_ctl = function
  | '\001' .. '\008' | '\011' | '\012' | '\014' .. '\031' | '\127' -> true
  | _ -> false

let is_qtext = function
  | '\033' | '\035' .. '\091' | '\093' .. '\126' -> true
  | c -> is_obs_no_ws_ctl c

module Type = struct
  type discrete = [ `Text | `Image | `Audio | `Video | `Application ]

  type composite = [ `Multipart ]

  type extension = [ `Ietf_token of string | `X_token of string ]

  type t = [ discrete | composite | extension ]

  let text = `Text

  let image = `Image

  let audio = `Audio

  let video = `Video

  let application = `Application

  let multipart = `Multipart

  let is_discrete = function #discrete -> true | _ -> false

  let is_multipart = function `Multipart -> true | _ -> false

  let ietf token = Ok (`Ietf_token token)

  let extension token =
    if String.length token < 3
    then
      Rresult.R.error_msgf "Extension token MUST have, at least, 3 bytes: %S"
        token
    else
      match (token.[0], token.[1]) with
      | ('x' | 'X'), '-' -> (
          try
            String.iter
              (fun chr -> if not (is_token chr) then raise Invalid_token)
              (String.sub token 2 (String.length token - 2)) ;
            Ok (`X_token token)
          with Invalid_token ->
            Rresult.R.error_msgf "Extension token %S does not respect standards"
              token)
      | _ ->
          Rresult.R.error_msgf "An extension token MUST be prefixed by [X-]: %S"
            token

  let pp ppf = function
    | `Text -> Fmt.string ppf "text"
    | `Image -> Fmt.string ppf "image"
    | `Audio -> Fmt.string ppf "audio"
    | `Video -> Fmt.string ppf "video"
    | `Application -> Fmt.string ppf "application"
    | `Multipart -> Fmt.string ppf "multipart"
    | `Ietf_token token -> Fmt.pf ppf "ietf:%s" token
    | `X_token token -> Fmt.pf ppf "x:%s" token

  let to_string = function
    | `Text -> "text"
    | `Image -> "image"
    | `Audio -> "audio"
    | `Video -> "video"
    | `Application -> "application"
    | `Multipart -> "multipart"
    | `Ietf_token token | `X_token token -> token

  let compare a b =
    String.(
      compare (lowercase_ascii (to_string a)) (lowercase_ascii (to_string b)))

  let equal a b = compare a b = 0

  let default = `Text
end

module Subtype = struct
  type t = [ `Ietf_token of string | `Iana_token of string | `X_token of string ]

  let ietf token =
    (* XXX(dinosaure): not sure how to check this value. *)
    Ok (`Ietf_token token)

  let iana token = Ok (`Iana_token token)

  let iana_exn token =
    match iana token with Ok v -> v | Error (`Msg err) -> invalid_arg err

  let v token = iana_exn token

  let extension token =
    if String.length token < 3
    then
      Rresult.R.error_msgf "Extension token MUST have, at least, 3 bytes: %S"
        token
    else
      match (token.[0], token.[1]) with
      | ('x' | 'X'), '-' -> (
          try
            String.iter
              (fun chr -> if not (is_token chr) then raise Invalid_token)
              (String.sub token 2 (String.length token - 2)) ;
            Ok (`X_token token)
          with Invalid_token ->
            Rresult.R.error_msgf "Extension token %S does not respect standards"
              token)
      | _ ->
          Rresult.R.error_msgf "An extension token MUST be prefixed by [X-]: %S"
            token

  let pp ppf = function
    | `Ietf_token token -> Fmt.pf ppf "ietf:%s" token
    | `Iana_token token -> Fmt.pf ppf "iana:%s" token
    | `X_token token -> Fmt.pf ppf "x:%s" token

  let compare a b =
    match (a, b) with
    | ( (`Ietf_token a | `Iana_token a | `X_token a),
        (`Ietf_token b | `Iana_token b | `X_token b) ) ->
        String.(compare (lowercase_ascii a) (lowercase_ascii b))

  let equal a b = compare a b = 0

  let default = `Iana_token "plain"
end

module Parameters = struct
  module Map = Map.Make (String)

  type key = string

  type value = String of string | Token of string

  type t = value Map.t

  let key key =
    (* XXX(dinosaure): RFC 2045 says:
       - attribute is ALWAYS case-insensitive
       - attribute := token
    *)
    try
      String.iter
        (fun chr -> if not (is_token chr) then raise Invalid_token)
        key ;
      Ok (String.lowercase_ascii key)
    with Invalid_token ->
      Rresult.R.error_msgf "Key %S does not respect standards" key

  let key_exn x =
    match key x with Ok v -> v | Error (`Msg err) -> invalid_arg err

  let k x = key_exn x

  exception Invalid_utf_8

  let value v =
    let to_token x =
      try
        String.iter
          (fun chr -> if not (is_token chr) then raise Invalid_token)
          x ;
        Ok (Token x)
      with Invalid_token ->
        Rresult.R.error_msgf "Value %S does not respect standards" v in

    (* XXX(dinosaure): [is_quoted_pair] accepts characters \000-\127. UTF-8
       extends to \000-\255. However, qtext invalids some of them: \009, \010,
       \013, \032, \034 and \092. Most of them need to be escaped.

       About \032, this case is little bit weird when [qcontent] accepts [FWS].
       At the end, \032, is possible in a quoted-string however, number of it
       does not look significant - so we don't try to escape it. *)
    let need_to_escape = function
      | '\009' | '\010' | '\013' | '\034' | '\092' -> true
      | _ -> false in
    let of_escaped_character = function
      | '\009' -> 't'
      | '\010' -> 'n'
      | '\013' -> 'r'
      | c -> c in
    let escape_characters x =
      let len = String.length x in
      let buf = Buffer.create len in
      String.iter
        (fun chr ->
          if need_to_escape chr
          then (
            Buffer.add_char buf '\\' ;
            Buffer.add_char buf (of_escaped_character chr))
          else Buffer.add_char buf chr)
        x ;
      Buffer.contents buf in
    let utf_8 x =
      try
        Uutf.String.fold_utf_8
          (fun () _pos -> function
            | `Malformed _ -> raise Invalid_utf_8
            | `Uchar _ -> ())
          () x ;
        Ok x
      with Invalid_utf_8 ->
        Rresult.R.error_msgf "Value %S is not a valid UTF-8 string" x in
    match to_token v with
    | Ok _ as v -> v
    | Error _ ->
        (* UTF-8 respects an interval of values and it's possible to have an
           invalid UTF-8 string. So we need to check it. UTF-8 is a superset of
           ASCII, so we need, firstly to check if it's a valid UTF-8 string. In
           this case, and mostly because we can escape anything (see
           [is_quoted_pair]), we do a pass to escape some of ASCII characters only
           then.

           At the end, if [value] is a valid UTF-8 string, we will don't have a
           problem to encode it if we take care to escape invalid [qtext]
           characters.

           However, order is really important semantically. UTF-8 -> escape
           expects a special process to decoder (escape -> UTF-8). About history,
           unicorn and so on, it should be the best to keep this order. *)
        Rresult.R.(utf_8 v >>| escape_characters >>| fun x -> String x)

  let value_exn x =
    match value x with Ok v -> v | Error (`Msg err) -> invalid_arg err

  let v x = value_exn x

  let empty = Map.empty

  let mem key t =
    (* XXX(dinosaure): [key] can only exist by [key] function which apply
       [String.lowercase_ascii]. *)
    Map.mem key t

  let add key value t = Map.add key value t

  let singleton key value = Map.singleton key value

  let remove key t = Map.remove key t

  let find key t =
    match Map.find key t with x -> Some x | exception Not_found -> None

  let iter f t = Map.iter f t

  let pp_key : key Fmt.t = Fmt.string

  let pp_value ppf = function
    | Token token -> Fmt.string ppf token
    | String value -> Fmt.pf ppf "%S" value

  let pp ppf t =
    let pp ppf (key, value) = Fmt.pf ppf "%a=%a" pp_key key pp_value value in
    Fmt.list ~sep:(Fmt.any ";@ ") pp ppf (Map.bindings t)

  let of_escaped_character = function
    | '\x61' -> '\x07' (* "\a" *)
    | '\x62' -> '\x08' (* "\b" *)
    | '\x74' -> '\x09' (* "\t" *)
    | '\x6E' -> '\x0A' (* "\n" *)
    | '\x76' -> '\x0B' (* "\v" *)
    | '\x66' -> '\x0C' (* "\f" *)
    | '\x72' -> '\x0D' (* "\r" *)
    | c -> c

  let value_unescape x =
    let len = String.length x in
    let res = Buffer.create len in
    let pos = ref 0 in
    while !pos < len do
      if x.[!pos] = '\\' && !pos < len - 1
         (* XXX(dinosaure): we can avoid this check when [value] takes care about that. *)
      then (
        Buffer.add_char res (of_escaped_character x.[!pos + 1]) ;
        pos := !pos + 2)
      else (
        Buffer.add_char res x.[!pos] ;
        incr pos)
    done ;
    Buffer.contents res

  let value_compare a b =
    match (a, b) with
    | Token a, Token b -> String.compare a b
    | String a, Token b | Token b, String a ->
        String.compare (value_unescape a) b
    | String a, String b -> String.compare (value_unescape a) (value_unescape b)

  let value_equal a b =
    match (a, b) with
    | Token a, Token b -> String.equal a b
    | String a, Token b | Token b, String a -> String.equal (value_unescape a) b
    | String a, String b -> String.equal (value_unescape a) (value_unescape b)

  let compare = Map.compare value_compare

  let equal = Map.equal value_equal

  let of_list lst =
    List.fold_left (fun a (key, value) -> Map.add key value a) Map.empty lst

  let to_list t = Map.bindings t

  let default = Map.add "charset" (Token "us-ascii") Map.empty
end

type t = {
  ty : Type.t;
  subty : Subtype.t;
  parameters : (string * Parameters.value) list;
}

let default =
  {
    ty = Type.default;
    subty = Subtype.default;
    parameters = Parameters.to_list Parameters.default;
  }

let ty { ty; _ } = ty

let subty { subty; _ } = subty

let parameters { parameters; _ } = parameters

let is_discrete { ty; _ } = Type.is_discrete ty

let is_multipart { ty; _ } = Type.is_multipart ty

let with_type : t -> Type.t -> t = fun t ty -> { t with ty }

let with_subtype : t -> Subtype.t -> t = fun t subty -> { t with subty }

let with_parameter : t -> Parameters.key * Parameters.value -> t =
 fun t (k, v) ->
  let parameters = Parameters.of_list ((k, v) :: t.parameters) in
  { t with parameters = Parameters.to_list parameters }

let boundary { parameters; _ } =
  match List.assoc_opt "boundary" parameters with
  | Some (Token v | String v) -> Some v
  | None -> None

let make ty subty parameters =
  { ty; subty; parameters = Parameters.to_list parameters }

let pp ppf { ty; subty; parameters } =
  Fmt.pf ppf "%a/%a %a" Type.pp ty Subtype.pp subty (Fmt.hvbox Parameters.pp)
    (Parameters.of_list parameters)

let equal a b =
  Type.equal a.ty b.ty
  && Subtype.equal a.subty b.subty
  && Parameters.(equal (of_list a.parameters) (of_list b.parameters))

module Decoder = struct
  open Angstrom

  let invalid_token token = Fmt.kstr fail "invalid token: %s" token

  let of_string s a =
    match parse_string ~consume:Consume.All a s with
    | Ok v -> Some v
    | Error _ -> None

  let is_wsp = function ' ' | '\t' -> true | _ -> false

  let token = take_while1 is_token

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

          discrete-type := "text" / "image" / "audio" / "video" /
                           "application" / extension-token
          composite-type := "message" / "multipart" / extension-token
          type := discrete-type / composite-type
  *)
  let ty =
    token >>= fun s ->
    (* XXX(dinosaure): lowercase_*ascii* is fine, not utf8 in this part. *)
    match String.lowercase_ascii s with
    | "text" -> return `Text
    | "image" -> return `Image
    | "audio" -> return `Audio
    | "video" -> return `Video
    | "application" -> return `Application
    | "multipart" -> return `Multipart
    | _ ->
    match of_string s extension_token with
    | Some v -> return v
    | None -> invalid_token s

  (* From RFC 2045

          subtype := extension-token / iana-token
  *)
  let subty =
    token >>= fun s ->
    try
      let v = `Iana_token s in
      return v
    with Not_found -> (
      match of_string s extension_token with
      | Some v -> return v
      | None -> invalid_token s)

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
    char '\\' *> any_char >>| Parameters.of_escaped_character >>| String.make 1

  let quoted_string =
    char '"' *> many (quoted_pair <|> utf_8_and is_qtext)
    <* char '"'
    >>| String.concat ""

  (* From RFC 2045

          value := token / quoted-string
  *)
  let value =
    quoted_string
    >>| (fun v -> Parameters.String v)
    <|> (token >>| fun v -> Parameters.Token v)

  (* From RFC 2045

          parameter := attribute "=" value
  *)
  let parameter =
    attribute >>= fun attribute ->
    skip_while is_wsp *> char '=' *> skip_while is_wsp *> value >>| fun value ->
    (attribute, value)

  (* From RFC 2045

          content := "Content-Type" ":" type "/" subtype
                     *(";" parameter)
                     ; Matching of media type and subtype
                     ; is ALWAYS case-insensitive.

     XXX(dinosaure): As others fields on mails, we consider WSP between
     tokens as RFC 822 said:

          Each header field can be viewed as a single, logical  line  of
          ASCII  characters,  comprising  a field-name and a field-body.
          For convenience, the field-body  portion  of  this  conceptual
          entity  can be split into a multiple-line representation; this
          is called "folding".  The general rule is that wherever  there
          may  be  linear-white-space  (NOT  simply  LWSP-chars), a CRLF
          immediately followed by AT LEAST one LWSP-char may instead  be
          inserted.

     NOTE: WSP is a replacement of CFWS token but it handles by
     [unstrctrd] and folded to WSP.
  *)
  let content =
    skip_while is_wsp *> ty <* skip_while is_wsp >>= fun ty ->
    char '/' >>= fun _ ->
    skip_while is_wsp *> subty <* skip_while is_wsp >>= fun subty ->
    many (skip_while is_wsp *> char ';' *> skip_while is_wsp *> parameter)
    >>| fun parameters -> { ty; subty; parameters }
end

let of_string str =
  let open Rresult in
  Unstrctrd.of_string str
  >>| (fun (_, v) -> Unstrctrd.fold_fws v)
  >>= Unstrctrd.without_comments
  >>| Unstrctrd.to_utf_8_string
  >>= fun str ->
  match Angstrom.parse_string ~consume:Prefix Decoder.content str with
  | Ok v -> Ok v
  | Error _ -> R.error_msgf "Invalid (unfolded) Content-Type value: %S" str

module Encoder = struct
  open Prettym

  let ty ppf = function
    | `Text -> string ppf "text"
    | `Image -> string ppf "image"
    | `Audio -> string ppf "audio"
    | `Video -> string ppf "video"
    | `Application -> string ppf "application"
    | `Message -> string ppf "message"
    | `Multipart -> string ppf "multipart"
    | `Ietf_token v -> string ppf v
    | `X_token v -> using (fun v -> "X-" ^ v) string ppf v

  let subty ppf = function
    | `Ietf_token v -> string ppf v
    | `Iana_token v -> string ppf v
    | `X_token v -> using (fun v -> "X-" ^ v) string ppf v

  let value ppf = function
    | Parameters.Token x -> eval ppf [ !!string ] x
    | Parameters.String x -> eval ppf [ char $ '"'; !!string; char $ '"' ] x

  let parameter ppf (key, v) =
    eval ppf [ box; !!string; cut; char $ '='; cut; !!value; close ] key v

  let parameters ppf parameters =
    let sep ppf () = eval ppf [ char $ ';'; fws ] in
    eval ppf [ box; !!(list ~sep:(sep, ()) parameter); close ] parameters

  let content_type ppf t =
    match t.parameters with
    | [] ->
        eval ppf
          [ bbox; !!ty; cut; char $ '/'; cut; !!subty; close ]
          t.ty t.subty
    | _ ->
        eval ppf
          [
            bbox;
            !!ty;
            cut;
            char $ '/';
            cut;
            !!subty;
            cut;
            char $ ';';
            fws;
            !!parameters;
            close;
          ]
          t.ty t.subty t.parameters
end

let to_string v = Prettym.to_string Encoder.content_type v
