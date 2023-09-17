(*{{{ Copyright (C) <2012> Anil Madhavapeddy <anil@recoil.org>
 * Copyright (C) <2009> David Sheets <sheets@alum.mit.edu>
 * Copyright (c) 2020, António Nuno Monteiro
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
  }}}*)

type expiration =
  [ `Session
  | `Max_age of int64
  ]

type same_site =
  [ `None
  | `Lax
  | `Strict
  ]

type cookie = string * string

module Set_cookie = struct
  type t =
    { cookie : cookie
    ; expiration : expiration
    ; domain : string option
    ; path : string option
    ; secure : bool
    ; http_only : bool
    ; same_site : same_site option
    }

  (* Does not check the contents of name or value for ';', ',', '\s', or
     name[0]='$' *)
  (* TODO: `SameSite=None` must also specify `Secure`:
   * https://web.dev/samesite-cookies-explained/ *)
  let make
      ?(expiration = `Session)
      ?path
      ?domain
      ?(secure = false)
      ?(http_only = false)
      ?same_site
      cookie
    =
    { cookie; expiration; domain; path; secure; http_only; same_site }

  let with_expiration t expiration = { t with expiration }
  let with_path t path = { t with path = Some path }
  let with_domain t domain = { t with domain = Some domain }
  let with_secure t secure = { t with secure }
  let with_http_only t http_only = { t with http_only }
  let with_same_site t same_site = { t with same_site = Some same_site }

  let serialize c =
    let attrs = if c.http_only then [ "httponly" ] else [] in
    let attrs = if c.secure then "secure" :: attrs else attrs in
    let attrs =
      match c.path with None -> attrs | Some p -> ("path=" ^ p) :: attrs
    in
    let attrs =
      match c.domain with None -> attrs | Some d -> ("domain=" ^ d) :: attrs
    in
    let attrs =
      match c.expiration with
      | `Session -> attrs
      | `Max_age age -> ("Max-Age=" ^ Int64.to_string age) :: attrs
    in
    let attrs =
      match c.same_site with
      | None -> attrs
      | Some same_site ->
        ("SameSite="
        ^
        match same_site with
        | `Lax -> "Lax"
        | `None -> "None"
        | `Strict -> "Strict")
        :: attrs
    in
    let n, c = c.cookie in
    let attrs = (n ^ "=" ^ c) :: attrs in
    "Set-Cookie", String.concat "; " attrs

  let parse alist cstr =
    let attrs = Stringext.split_trim_left cstr ~on:",;" ~trim:" \t" in
    let attrs =
      List.map
        (fun attr ->
           match Stringext.split ~on:'=' attr with
           | [] -> "", ""
           | n :: v -> n, String.concat "=" v)
        attrs
    in
    try
      let cookie = List.hd attrs in
      let attrs =
        List.map (fun (n, v) -> String.lowercase_ascii n, v) (List.tl attrs)
      in
      (* TODO: expires *)
      let expiration =
        try
          let v = List.assoc "max-age" attrs in
          match Int64.of_string_opt v with
          | Some v -> `Max_age v
          | None -> `Session
        with
        | Not_found -> `Session
      in
      let path =
        try
          let v = List.assoc "path" attrs in
          if v = "" || v.[0] <> '/' then raise Not_found else Some v
        with
        | Not_found -> None
      in
      let domain =
        try
          let v = List.assoc "domain" attrs in
          if v = ""
          then raise Not_found
          else
            Some
              (String.lowercase_ascii
                 (if v.[0] = '.' then Stringext.string_after v 1 else v))
        with
        | Not_found -> None
      in
      let same_site =
        try
          let v = List.assoc "samesite" attrs in
          match String.lowercase_ascii v with
          | "lax" -> Some `Lax
          | "strict" -> Some `Strict
          | "none" -> Some `None
          | _ -> None
        with
        | Not_found -> None
      in
      (* TODO: trim wsp *)
      ( fst cookie
      , { cookie
        ; expiration
        ; domain
        ; path
        ; http_only = List.mem_assoc "httponly" attrs
        ; secure = List.mem_assoc "secure" attrs
        ; same_site
        } )
      :: alist
    with
    | Failure _ -> alist

  (* TODO: check dupes+order *)
  let parse headers =
    List.fold_left parse [] (Headers.get_multi headers "set-cookie")

  let key { cookie = k, _; _ } = k
  let value { cookie = _, v; _ } = v
end

module Cookie = struct
  (* RFC 2965 has
   * cookie          =  "Cookie:" cookie-version 1*((";" | ",") cookie-value)
   * cookie-value    =  NAME "=" VALUE [";" path] [";" domain] [";" port]
   * cookie-version  =  "$Version" "=" value
   * NAME            =  attr
   * VALUE           =  value
   * path            =  "$Path" "=" value
   * domain          =  "$Domain" "=" value
   * port            =  "$Port" [ "=" <"> value <"> ]
   *)

  let parse headers =
    List.fold_left
      (fun acc header ->
         let comps = Stringext.split_trim_left ~on:";" ~trim:" \t" header in
         (* We don't handle $Path, $Domain, $Port, $Version (or $anything
            $else) *)
         let cookies =
           List.filter (fun s -> String.length s > 0 && s.[0] != '$') comps
         in
         let split_pair nvp =
           match Stringext.split ~on:'=' nvp ~max:2 with
           | [] -> "", ""
           | [ n ] -> n, ""
           | n :: v :: _ -> n, v
         in
         List.map split_pair cookies @ acc)
      []
      (Headers.get_multi headers "cookie")

  let serialize cookies =
    "cookie", String.concat "; " (List.map (fun (k, v) -> k ^ "=" ^ v) cookies)
end
