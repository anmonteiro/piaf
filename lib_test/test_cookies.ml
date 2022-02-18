module Set_cookie = struct
  include Piaf.Cookies.Set_cookie

  let pp_hum fmt t =
    let k, v = Piaf.Cookies.Set_cookie.serialize t in
    Format.fprintf fmt "%S=%S" k v
end

let set_cookie = Alcotest.of_pp Set_cookie.pp_hum

let parse_one cookie_str =
  Set_cookie.parse (Piaf.Headers.of_list [ "set-cookie", cookie_str ])
  |> List.hd
  |> snd

let test_set_cookie () =
  let jwt_payload =
    "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJjIjoiezppc3N1ZWRcL3RpbWUgI2luc3QgXCIyMDIwLTAyLTAyVDAzOjQ1OjA2LjgxOS0wMDowMFwiLCA6dXNlclwvZ3Vlc3Q_IHRydWUsIDp1c2VyXC9pZCAjdXVpZCBcIjEwMTFiMWYyLTRkMGMtNDk0NC1iYjA2LTdmYzE4YWMzMWQ2N1wifSJ9.khdb4tCjbCjcurorqsBB_3qpOAG8MlRTmn7GnjLQYlU"
  in
  let example =
    Format.asprintf "auth=%s;Path=/; SameSite=Lax;Secure;HttpOnly" jwt_payload
  in
  Alcotest.check
    set_cookie
    "parse samesite=lax"
    (Set_cookie.make
       ~path:"/"
       ~secure:true
       ~http_only:true
       ~same_site:`Lax
       ("auth", jwt_payload))
    (parse_one example)

let suite = [ "set cookie", `Quick, test_set_cookie ]
let () = Alcotest.run "cookie unit tests" [ "cookies", suite ]
