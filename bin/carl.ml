open Async
open Lwt.Infix
open Cmdliner

let setup_log ?style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let request ~config ~meth host =
  let headers = [ "user-agent", "carl/0.0.0-experimental" ] in
  Piaf.Client.request ~config ~meth ~headers (Uri.of_string host) >|= function
  | Ok _response ->
    `Ok ()
  | Error e ->
    `Error (false, e)

let log_level_of_list = function [] -> Logs.App | [ _ ] -> Info | _ -> Debug

type cli =
  { follow_redirects : bool
  ; max_redirects : int
  ; meth : Piaf.Method.t
  ; log_level : Logs.level
  ; urls : string list
  }

let main { follow_redirects; max_redirects; meth; log_level; urls } =
  setup_log (Some log_level);
  let config =
    { Piaf.Config.default_config with follow_redirects; max_redirects }
  in
  (* TODO: Issue requests for all URLs *)
  let x = List.hd urls in
  Lwt_main.run (request ~config ~meth x)

(* -H / --header
 * -d / --data
 * --retry
 * --compressed
 * --connect-timeout
 * -i / --include
 * -I / --head
 * -k / --insecure
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

  let parse follow_redirects max_redirects request verbose urls =
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
    }

  let default_cmd =
    Term.(
      const parse $ follow_redirects $ max_redirects $ request $ verbose $ urls)

  let cmd =
    let doc = "Like curl, for caml" in
    Term.(ret (const main $ default_cmd)), Term.info "carl" ~version:"todo" ~doc
end

let () = Term.(exit @@ eval CLI.cmd)
