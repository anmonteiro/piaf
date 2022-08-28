let src = Logs.Src.create "piaf.server" ~doc:"Piaf Server module"

module Log = (val Logs.src_log src : Logs.LOG)

let report_exn
    : type reqd.
      (module Http_intf.HTTPServerCommon with type Reqd.t = reqd)
      -> reqd
      -> exn
      -> unit
  =
 fun (module Http) reqd exn ->
  Log.err (fun m ->
      let raw_backtrace = Printexc.get_raw_backtrace () in
      m
        "Exception while handling request: %s.@]@;<0 2>@[<v 0>%a@]"
        (Printexc.to_string exn)
        Util.Backtrace.pp_hum
        raw_backtrace);
  Http.Reqd.report_exn reqd exn
