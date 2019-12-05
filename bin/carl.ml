open Async
open Lwt.Infix
open Cmdliner

let setup_log ?style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let request ~config host =
  Piaf.Client.get ~config (Uri.of_string host) >|= function
  | Ok _response ->
    `Ok ()
  | Error e ->
    `Error (false, e)

let verbosity_of_list = function [] -> Logs.App | [ _ ] -> Info | _ -> Debug

let main follow_redirects max_redirects verbose urls =
  let verbosity = verbosity_of_list verbose in
  setup_log (Some verbosity);
  let config =
    { Piaf.Config.default_config with follow_redirects; max_redirects }
  in
  let x = List.hd urls in
  Lwt_main.run (request ~config x)

module CLI = struct
  let verbose =
    let doc = "Verbosity (use multiple times to increase)" in
    Arg.(value & flag_all & info [ "v"; "verbose" ] ~doc)

  let follow_redirects =
    let doc = "Follow redirects" in
    Arg.(value & flag & info [ "L"; "location" ] ~doc)

  let max_redirects =
    let doc = "Max number of redirects to follow" in
    Arg.(
      value
      & opt int Piaf.Config.default_config.max_redirects
      & info [ "max-redirs" ] ~doc)

  let urls =
    let docv = "URLs" in
    Arg.(non_empty & pos_all string [] & info [] ~docv)

  let cmd =
    let doc = "Like curl, for caml" in
    ( Term.(ret (const main $ follow_redirects $ max_redirects $ verbose $ urls))
    , Term.info "carl" ~version:"todo" ~doc )
end

let () = Term.(exit @@ eval CLI.cmd)
