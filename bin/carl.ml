open Async
open Cmdliner

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
  Logs.set_level level;
  Logs.set_reporter format_reporter

let request ~config ~meth uri =
  let open Lwt.Syntax in
  let headers = [ "user-agent", "carl/0.0.0-experimental" ] in
  let* res = Piaf.Client.request ~config ~meth ~headers uri in
  match res with
  | Ok (_response, response_body) ->
    let+ () =
      Lwt_stream.iter_s
        (fun body_fragment -> Logs_lwt.app (fun m -> m "%s" body_fragment))
        response_body
    in
    `Ok ()
  | Error e ->
    Lwt.return (`Error (false, e))

let log_level_of_list = function [] -> Logs.App | [ _ ] -> Info | _ -> Debug

type cli =
  { follow_redirects : bool
  ; max_redirects : int
  ; meth : Piaf.Method.t
  ; log_level : Logs.level
  ; urls : string list
  ; default_proto : string
  }

let rec uri_of_string ~scheme s =
  let maybe_uri = Uri.of_string s in
  match Uri.host maybe_uri, Uri.scheme maybe_uri with
  | None, _ ->
    (* If Uri.of_string didn't get a host it must mean that the scheme wasn't
     * even present. *)
    Logs.debug (fun m ->
        m "Protocol not provided for %s. Using the default scheme: %s" s scheme);
    uri_of_string ~scheme ("//" ^ s)
  | Some _, None ->
    (* e.g. `//example.com` *)
    Uri.with_scheme maybe_uri (Some scheme)
  | Some _, Some _ ->
    maybe_uri

let main
    { follow_redirects; max_redirects; meth; log_level; urls; default_proto }
  =
  setup_log (Some log_level);
  let config =
    { Piaf.Config.default_config with follow_redirects; max_redirects }
  in
  (* TODO: Issue requests for all URLs *)
  let uri = uri_of_string ~scheme:default_proto (List.hd urls) in
  Lwt_main.run (request ~config ~meth uri)

(* -H / --header
 * -d / --data
 * --retry
 * --compressed
 * --connect-timeout
 * -i / --include
 * -I / --head
 * -k / --insecure
 * --http0.9       Allow HTTP 0.9 responses
 * -0 / --http1.0  Use HTTP 1.0
 * --http1.1       Use HTTP 1.1
 * --http2         Use HTTP 2
 * --http2-prior-knowledge Use HTTP 2 without HTTP/1.1 Upgrade
 *)
module CLI = struct
  let request =
    let request_conv =
      let parse meth = Result.Ok (Piaf.Method.of_string meth) in
      let print = Piaf.Method.pp_hum in
      Arg.conv ~docv:"method" (parse, print)
    in
    let doc = "Specify request method to use" in
    let docv = "method" in
    Arg.(
      value & opt (some request_conv) None & info [ "X"; "request" ] ~doc ~docv)

  let default_proto =
    let doc = "Use $(docv) for any URL missing a scheme (without `://`)" in
    let docv = "protocol" in
    Arg.(value & opt string "http" & info [ "proto-default" ] ~doc ~docv)

  let follow_redirects =
    let doc = "Follow redirects" in
    Arg.(value & flag & info [ "L"; "location" ] ~doc)

  let max_redirects =
    let doc = "Max number of redirects to follow" in
    let docv = "int" in
    Arg.(
      value
      & opt int Piaf.Config.default_config.max_redirects
      & info [ "max-redirs" ] ~doc ~docv)

  let verbose =
    let doc = "Verbosity (use multiple times to increase)" in
    Arg.(value & flag_all & info [ "v"; "verbose" ] ~doc)

  let urls =
    let docv = "URLs" in
    Arg.(non_empty & pos_all string [] & info [] ~docv)

  let parse default_proto follow_redirects max_redirects request verbose urls =
    { follow_redirects
    ; max_redirects
    ; meth =
        (match request with
        | None ->
          (* TODO: default to `POST` when we implement --data *)
          `GET
        | Some meth ->
          meth)
    ; log_level = log_level_of_list verbose
    ; urls
    ; default_proto
    }

  let default_cmd =
    Term.(
      const parse
      $ default_proto
      $ follow_redirects
      $ max_redirects
      $ request
      $ verbose
      $ urls)

  let cmd =
    let doc = "Like curl, for caml" in
    Term.(ret (const main $ default_cmd)), Term.info "carl" ~version:"todo" ~doc
end

let () = Term.(exit @@ eval CLI.cmd)
