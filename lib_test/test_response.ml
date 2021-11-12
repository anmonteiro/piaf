open Lwt.Syntax
open Piaf

let test_of_file _ () =
  let+ response = Response.of_file "./test_response.ml" in
  Alcotest.(check string)
    "expected status 200"
    "200"
    (Status.to_string (Result.get_ok response).status);
  Alcotest.(check (Alcotest.of_pp Headers.pp_hum))
    "expected header"
    (Headers.of_list [ Headers.Well_known.content_type, "text/x-ocaml" ])
    (Result.get_ok response).headers

let test_of_file_nonexistent _ () =
  let+ response = Response.of_file "./does_not_exist.ml" in
  Alcotest.(
    check
      (result (Alcotest.of_pp Response.pp_hum) (Alcotest.of_pp Error.pp_hum)))
    "expected error"
    (Error (`Exn (Unix.Unix_error (Unix.ENOENT, "open", "./does_not_exist.ml"))))
    response

let suite =
  [ ( "response"
    , List.map
        (fun (desc, ty, f) -> Alcotest_lwt.test_case desc ty f)
        [ "of_file", `Quick, test_of_file
        ; "non-existent of_file", `Quick, test_of_file_nonexistent
        ] )
  ]

let () =
  let setup_log ?style_renderer level =
    let pp_header src ppf (l, h) =
      if l = Logs.App then
        Format.fprintf ppf "%a" Logs_fmt.pp_header (l, h)
      else
        let x =
          match Array.length Sys.argv with
          | 0 ->
            Filename.basename Sys.executable_name
          | _n ->
            Filename.basename Sys.argv.(0)
        in
        let x =
          if Logs.Src.equal src Logs.default then
            x
          else
            Logs.Src.name src
        in
        Format.fprintf ppf "%s: %a " x Logs_fmt.pp_header (l, h)
    in
    let format_reporter =
      let report src =
        let { Logs.report } = Logs_fmt.reporter ~pp_header:(pp_header src) () in
        report src
      in
      { Logs.report }
    in
    Fmt_tty.setup_std_outputs ?style_renderer ();
    Logs.set_level ~all:true (Some level);
    Logs.set_reporter format_reporter
  in
  setup_log Debug;
  Lwt_main.run (Alcotest_lwt.run "Piaf client tests" suite)
