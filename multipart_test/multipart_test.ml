open Eio.Std

let content_type =
  "multipart/form-data; boundary=----WebKitFormBoundaryVuTaDGWRcduyfmAv"

let multipart_request_body n =
  Format.asprintf
    {|------WebKitFormBoundaryVuTaDGWRcduyfmAv
Content-Disposition: form-data; name="picture.png"; filename="picture.png"
Content-Type: image/png

%s
------WebKitFormBoundaryVuTaDGWRcduyfmAv--
|}
    (String.make n 'a')

let multipart_request_body_chunks payload =
  let continue = ref true in
  let cur_off = ref 0 in
  let ret = ref [] in
  let chunk_size = 32 in
  while !continue do
    let this_len = min chunk_size (String.length payload - !cur_off) in
    let subs = String.sub payload !cur_off this_len in
    ret := subs :: !ret;
    cur_off := !cur_off + this_len;
    if !cur_off = String.length payload then continue := false
  done;
  List.rev !ret

let test_simple_boundary ~sw _env () =
  let payload_size = 0x1000 in
  let max_chunk_size = 0x400 in
  let chunks =
    multipart_request_body_chunks (multipart_request_body payload_size)
  in
  let stream, push = Stream.create 1024 in
  List.iter
    (fun x ->
      push
        (Some
           { Faraday.buffer =
               Bigstringaf.of_string ~off:0 ~len:(String.length x) x
           ; off = 0
           ; len = String.length x
           }))
    chunks;
  push None;
  let waiter, wakener = Promise.create () in
  let emit name stream = Promise.resolve wakener (name, stream) in
  let multipart_result =
    Fiber.fork_promise ~sw (fun () ->
      Multipart.parse_multipart_form
         ~content_type
         ~max_chunk_size
         ~emit
         stream)
  in
  let name, stream = Promise.await waiter in
  Alcotest.(check (option string))
    "filename extracted"
    (Some "picture.png")
    name;
  Alcotest.(check bool)
    "working through chunks in parallel (stream arrives before the promise \
     resolves)"
    false
    ((Promise.is_resolved multipart_result));
  let chunks = Stream.to_list stream in
  Alcotest.(check int)
    "Correct number of chunks emitted"
    (payload_size / max_chunk_size)
    (List.length chunks);
  match Promise.await multipart_result with
  | Ok (Ok t) ->
    let fields = Multipart.result_fields t in
    Alcotest.(check int) "one field" 1 (List.length fields);
    let name, multipart_fields = List.hd fields in
    Alcotest.(check string) "field name" "picture.png" name;
    Alcotest.(check string)
      "parsed content-type and disposition correctly"
      {|Content-Disposition[*]: { type= <ietf:form-data>; filename= picture.png;
                          creation= ; modification= ; read= ; size= ;
                          parameters= (parameters (name, "picture.png")); }
Content-Type[*]: image/iana:png |}
      (Format.asprintf "%a" Multipart_form.Header.pp multipart_fields)
  | Ok (Error (`Msg msg)) ->
    Alcotest.fail msg
  | Error (exn) ->
    Alcotest.fail (Printexc.to_string exn)

let test_unaligned_boundary ~sw _env () =
  let payload_size = 0x1100 in
  let max_chunk_size = 0x400 in
  let chunks =
    multipart_request_body_chunks (multipart_request_body payload_size)
  in
  let stream, push = Stream.create 1024 in
  List.iter
    (fun x ->
      push
        (Some
           { Faraday.buffer =
               Bigstringaf.of_string ~off:0 ~len:(String.length x) x
           ; off = 0
           ; len = String.length x
           }))
    chunks;
  push None;
  let waiter, wakener = Promise.create () in
  let emit name stream = Promise.resolve wakener (name, stream) in
  let _multipart_result =
    Fiber.fork_promise ~sw (fun () ->
    Multipart.parse_multipart_form ~content_type ~max_chunk_size ~emit stream
    )
  in
  let name, stream = Promise.await waiter in
  Alcotest.(check (option string))
    "filename extracted"
    (Some "picture.png")
    name;
  let chunks = Stream.to_list stream in
  Alcotest.(check int)
    "Correct number of chunks emitted"
    ((payload_size / max_chunk_size) + 1)
    (List.length chunks)

let test_no_boundary ~sw:_ _env () =
  let content_type_no_boundary = "text/plain" in
  let request_body n = String.make n 'a' in
  let payload_size = 0x1000 in
  let max_chunk_size = 0x400 in
  let chunks = multipart_request_body_chunks (request_body payload_size) in
  let stream, push = Stream.create 1024 in
  List.iter
    (fun x ->
      push
        (Some
           { Faraday.buffer =
               Bigstringaf.of_string ~off:0 ~len:(String.length x) x
           ; off = 0
           ; len = String.length x
           }))
    chunks;
  push None;
  let waiter, wakener = Promise.create () in
  let emit name stream = Promise.resolve wakener (name, stream) in
  let multipart_result =
    Multipart.parse_multipart_form
      ~content_type:content_type_no_boundary
      ~max_chunk_size
      ~emit
      stream
  in
  ignore @@ Result.get_ok multipart_result;
  let name, stream = Promise.await waiter in
  Alcotest.(check (option string)) "filename extracted" None name;
  let chunks = Stream.to_list stream in
  Alcotest.(check int)
    "Correct number of chunks emitted"
    (payload_size / max_chunk_size)
    (List.length chunks)

let test_no_boundary_unaligned ~sw:_ _env () =
  let content_type_no_boundary = "text/plain" in
  let request_body n = String.make n 'a' in
  let payload_size = 0x1100 in
  let max_chunk_size = 0x400 in
  let chunks = multipart_request_body_chunks (request_body payload_size) in
  let stream, push = Stream.create 1024  in
  List.iter
    (fun x ->
      push
        (Some
           { Faraday.buffer =
               Bigstringaf.of_string ~off:0 ~len:(String.length x) x
           ; off = 0
           ; len = String.length x
           }))
    chunks;
  push None;
  let waiter, wakener = Promise.create () in
  let emit name stream = Promise.resolve wakener (name, stream) in
  let multipart_result =
    Multipart.parse_multipart_form
      ~content_type:content_type_no_boundary
      ~max_chunk_size
      ~emit
      stream
  in
  ignore @@ Result.get_ok multipart_result;
  let name, stream = Promise.await waiter in
  Alcotest.(check (option string)) "filename extracted" None name;
  let chunks = Stream.to_list stream in
  Alcotest.(check int)
    "Correct number of chunks emitted"
    ((payload_size / max_chunk_size) + 1)
    (List.length chunks)

let test_no_boundary_but_boundary_expected ~sw:_ _env () =
  let content_type_no_boundary = "multipart/form-data" in
  let request_body n = String.make n 'a' in
  let payload_size = 0x1100 in
  let max_chunk_size = 0x400 in
  let chunks = multipart_request_body_chunks (request_body payload_size) in
  let stream, push = Stream.create 1024  in
  List.iter
    (fun x ->
      push
        (Some
           { Faraday.buffer =
               Bigstringaf.of_string ~off:0 ~len:(String.length x) x
           ; off = 0
           ; len = String.length x
           }))
    chunks;
  push None;
  let emit _ _ = assert false in
  let multipart_result =
    Multipart.parse_multipart_form
      ~content_type:content_type_no_boundary
      ~max_chunk_size
      ~emit
      stream
  in
  match multipart_result with
  | Ok _ ->
    Alcotest.fail "expected failure"
  | Error _ ->
    Alcotest.(check pass) "failed because boundary was expected" () ()

let test_case
    :  string -> Alcotest.speed_level
    -> (sw:Switch.t -> Eio.Stdenv.t -> unit -> unit)
    -> string * Alcotest.speed_level * (unit -> unit)
  =
 fun desc ty f ->
  ( desc
  , ty
  , fun () -> Eio_main.run (fun env -> Switch.run (fun sw -> f ~sw env ())) )

let suite =
  [ ( "multipart"
    , List.map
        (fun (desc, ty, f) -> test_case desc ty f)
        [ "simple boundary", `Quick, test_simple_boundary
        ; "unaligned boundary", `Quick, test_unaligned_boundary
        ; "no boundary", `Quick, test_no_boundary
        ; "no boundary, unaligned", `Quick, test_no_boundary_unaligned
        ; ( "no boundary but boundary expected"
          , `Quick
          , test_no_boundary_but_boundary_expected )
        ] )
  ]

let () =  Alcotest.run "Multipart form unit tests" suite
