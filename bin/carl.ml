(*----------------------------------------------------------------------------
 * Copyright (c) 2019-2020, António Nuno Monteiro
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *---------------------------------------------------------------------------*)

open Eio.Std
open Cmdliner
open Piaf

let setup_log ?style_renderer level =
  let pp_header src ppf (l, h) =
    if l = Logs.App
    then Format.fprintf ppf "%a" Logs_fmt.pp_header (l, h)
    else
      let x =
        match Array.length Sys.argv with
        | 0 -> Filename.basename Sys.executable_name
        | _n -> Filename.basename Sys.argv.(0)
      in
      let x =
        if Logs.Src.equal src Logs.default then x else Logs.Src.name src
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
  Logs.set_level ~all:true level;
  Logs.set_reporter format_reporter

type output =
  | Stdout
  | Channel of string

type data =
  | File of string
  | Data of string

type cli =
  { follow_redirects : bool
  ; max_redirects : int
  ; meth : Method.t
  ; log_level : Logs.level option
  ; urls : string list
  ; data : data option
  ; default_proto : string
  ; head : bool
  ; headers : (string * string) list
  ; include_ : bool
  ; max_http_version : Versions.HTTP.t
  ; h2c_upgrade : bool
  ; http2_prior_knowledge : bool
  ; cacert : Cert.t option
  ; capath : string option
  ; clientcert : (Cert.t * Cert.t) option
  ; min_tls_version : Versions.TLS.t
  ; max_tls_version : Versions.TLS.t
  ; insecure : bool
  ; user_agent : string
  ; connect_timeout : float
  ; referer : string option
  ; compressed : bool
  ; user : string option
  ; oauth2_bearer : string option
  ; output : output
  }

let format_header formatter (name, value) =
  Format.fprintf formatter "%a: %s" Fmt.(styled `Bold string) name value

let pp_response_headers formatter { Response.headers; status; version; _ } =
  let reason_phrase =
    match status with
    | #Status.standard as st ->
      Format.asprintf " %s" (Status.default_reason_phrase st)
    | `Code _ -> ""
  in
  Format.fprintf
    formatter
    "@[%a %a%s@]@\n@[%a@]"
    Versions.HTTP.pp
    version
    Status.pp_hum
    status
    reason_phrase
    (Format.pp_print_list
       ~pp_sep:(fun f () -> Format.fprintf f "@\n")
       format_header)
    (Headers.to_list headers)

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
  | Some _, Some _ -> maybe_uri

module Ansi = struct
  let clear_line = "\u{1B}[2K"
  let line_up = "\u{1B}[A"

  let dumb =
    try match Sys.getenv "TERM" with "dumb" | "" -> true | _ -> false with
    | Not_found -> true

  let isatty = try Unix.(isatty stdout) with Unix.Unix_error _ -> false
end

let ok = Ok ()

let print_string ~cli formatter s =
  if Ansi.isatty && cli.output = Stdout && String.contains s '\000'
  then (
    let msg =
      "Binary output can mess up your terminal. Use \"--output -\" to tell \
       carl to output it to your terminal anyway, or consider \"--output \
       <FILE>\" to save to a file."
    in
    Logs.warn (fun m -> m "%s" msg);
    Error (`Msg msg))
  else (
    Format.fprintf formatter "%s" s;
    Format.pp_print_flush formatter ();
    ok)

module Size = struct
  let gb = int_of_float (1024. ** 3.)
  let mb = int_of_float (1024. ** 2.)
  let kb = 1024
end

let report_progess ?(first = false) ~cli len total_len =
  match cli.output, total_len with
  | Channel "-", _ -> ()
  | Stdout, _ when Unix.isatty Unix.stdout -> ()
  | (Stdout | Channel _), `Fixed total_len ->
    let total_bars = 40. in
    let pct_complete = Int64.(to_float (div (mul len 100L) total_len)) in
    (* We show 40 bars *)
    let bars = ceil (pct_complete /. (100. /. total_bars)) in
    let spaces = total_bars -. bars in
    Format.eprintf
      "%s[%s%s] %d%%@\n%!"
      (if not first then Ansi.clear_line ^ Ansi.line_up else Ansi.clear_line)
      (String.concat "" (List.init (int_of_float bars) (fun _ -> "\u{25a0}")))
      (String.make (int_of_float spaces) ' ')
      (int_of_float pct_complete)
  | (Stdout | Channel _), (`Chunked | `Unknown | `Close_delimited) ->
    let len = Int64.to_int len in
    let len, unit =
      match len with
      | len when len >= Size.gb -> float_of_int len /. float_of_int Size.gb, "G"
      | len when len >= Size.mb -> float_of_int len /. float_of_int Size.mb, "M"
      | len when len >= Size.kb -> float_of_int len /. float_of_int Size.kb, "k"
      | _ -> float_of_int len, "B"
    in
    Format.eprintf
      "%s Transferred:@;<0 3>@[<v 0>%.2f%s@]@\n%!"
      (if not first
       then Ansi.clear_line ^ Ansi.line_up ^ Ansi.clear_line ^ Ansi.line_up
       else Ansi.clear_line)
      len
      unit
  | _ -> ()

let inflate_chunk zstream result_buffer chunk =
  let buf_size = 1024 in
  let buf = Bytes.create buf_size in
  let rec inner ~off ~len =
    let is_end, used_in, used_out =
      Zlib.inflate_string zstream chunk off len buf 0 buf_size Zlib.Z_SYNC_FLUSH
    in
    Buffer.add_subbytes result_buffer buf 0 used_out;
    match is_end, used_in < len with
    | true, _ ->
      assert (used_in <= len);
      Zlib.inflate_end zstream
    | false, true -> inner ~off:(off + used_in) ~len:(len - used_in)
    | false, false -> ()
  in
  inner ~off:0 ~len:(String.length chunk)

(* TODO: try / catch *)
let inflate response_body =
  let open Util.Result.Syntax in
  let zstream = Zlib.inflate_init false in
  let result_buf = Buffer.create 1024 in
  let+ () =
    Body.iter_string
      ~f:(fun chunk -> inflate_chunk zstream result_buf chunk)
      response_body
  in
  Buffer.contents result_buf

let handle_response
    ~cli
    ~sw
    ~(stdout : _ Eio_unix.sink)
    ({ Response.body; _ } as response)
  =
  let open Util.Result.Syntax in
  let { head; compressed; include_; _ } = cli in
  let* channel, formatter =
    match cli.output with
    | Stdout | Channel "-" ->
      Ok (Eio_unix.Resource.fd_opt stdout |> Option.get, Format.std_formatter)
    | Channel filename ->
      (try
         Eio_unix.run_in_systhread (fun () ->
             let fd =
               Unix.openfile
                 filename
                 Unix.[ O_NONBLOCK; O_WRONLY; O_TRUNC; O_CREAT ]
                 0o600
             in
             let fd = Eio_unix.Fd.of_unix ~sw ~close_unix:true fd in
             Ok
               ( fd
               , Format.formatter_of_out_channel
                   (Unix.out_channel_of_descr
                      (Eio_unix.Fd.use_exn "handle_response" fd Fun.id)) ))
       with
      | exn -> Error (`Exn exn))
  in
  if head || include_
  then (
    Format.fprintf formatter "%a@." pp_response_headers response;
    Format.pp_print_flush formatter ());
  let result =
    if head
    then (
      Fiber.fork ~sw (fun () ->
          let (_ : (unit, _) result) = Body.drain body in
          ());
      ok)
    else
      match compressed, Headers.get response.headers "content-encoding" with
      | true, Some encoding when String.lowercase_ascii encoding = "gzip" ->
        (* We requested a compressed response, and we got a compressed response
         * back. *)
        let* response_body_str = Body.to_string body in
        (match Ezgzip.decompress response_body_str with
        | Ok body_str -> print_string ~cli formatter body_str
        | Error _ -> assert false)
      | true, Some encoding when String.lowercase_ascii encoding = "deflate" ->
        (* We requested a compressed response, and we got a compressed response
         * back. *)
        let* s = inflate body in
        print_string ~cli formatter s
      | _ ->
        let stream = Body.to_stream body in
        let total_len = Body.length body in
        (try
           match Stream.take stream with
           | Some { Piaf.IOVec.buffer; off; len } ->
             let running_total = Int64.of_int len in
             report_progess ~first:true ~cli running_total total_len;
             let chunk = Bigstringaf.substring buffer ~off ~len in
             let* () = print_string ~cli formatter chunk in
             let+ (_ret : int64) =
               Body.fold
                 ~f:(fun running_total { Piaf.IOVec.buffer; off; len } ->
                   let new_total = Int64.(add (of_int len) running_total) in
                   report_progess ~cli new_total total_len;
                   let body_fragment = Bigstringaf.substring buffer ~off ~len in
                   let (_ : _ result) =
                     print_string ~cli formatter body_fragment
                   in
                   new_total)
                 ~init:running_total
                 body
             in
             ()
           | None -> assert false
         with
        | exn -> Error (`Exn exn))
  in
  (match cli.output with
  | Stdout | Channel "-" -> Format.pp_print_newline formatter ()
  | Channel _ ->
    Eio_unix.run_in_systhread (fun () ->
        Unix.close (Eio_unix.Fd.use_exn "close" channel Fun.id)));
  result

let build_headers
    ~cli:{ headers; user_agent; referer; compressed; oauth2_bearer; user; _ }
  =
  let headers = ("User-Agent", user_agent) :: headers in
  let headers =
    match referer with
    | None -> headers
    | Some referer -> ("Referer", referer) :: headers
  in
  let headers =
    if compressed
    then ("Accept-Encoding", "deflate, gzip") :: headers
    else headers
  in
  match oauth2_bearer, user with
  | Some token, _ ->
    Logs.debug (fun m ->
        let user = match user with Some user -> user | None -> "''" in
        m "Server authorization using Bearer with user %s" user);
    (* Bearer token overrides `user` *)
    ("Authorization", "Bearer " ^ token) :: headers
  | None, Some user ->
    Logs.debug (fun m -> m "Server authorization using Basic with user %s" user);
    ("Authorization", "Basic " ^ Base64.encode_exn user) :: headers
  | None, None -> headers

let request env ~sw ~cli ~config uri =
  let module Client = Client.Oneshot in
  let { meth; data; _ } = cli in
  let uri_user = Uri.userinfo uri in
  let cli =
    match cli.user with
    | None -> { cli with user = uri_user }
    | Some _ ->
      (* `-u` overrides the URI userinfo *)
      cli
  in
  let headers = build_headers ~cli in
  let body =
    match data with
    | Some (Data s) -> Some (Body.of_string s)
    | Some (File filename) ->
      let fd = Unix.openfile filename [ O_RDONLY ] 0 in
      let { Unix.st_size = length; _ } =
        Eio_unix.run_in_systhread (fun () -> Unix.fstat fd)
      in
      let remaining = ref length in
      let flow = Eio_unix.Net.import_socket_stream ~sw ~close_unix:true fd in
      let stream =
        Stream.from ~f:(fun () ->
            if !remaining = 0
            then None
            else
              let len = min config.Config.body_buffer_size !remaining in
              let cs = Cstruct.create len in
              Eio.Flow.read_exact flow cs;
              remaining := !remaining - len;
              Some { Faraday.buffer = cs.buffer; off = cs.off; len = cs.len })
      in
      Fiber.fork ~sw (fun () ->
          Stream.when_closed ~f:(fun () -> Eio.Flow.close flow) stream);
      let body_length =
        match
          List.find_opt
            (fun (nm, _) -> String.lowercase_ascii nm = "transfer-encoding")
            headers
        with
        | Some (_, "chunked") -> `Chunked
        | _ -> `Fixed (Int64.of_int length)
      in
      Some (Body.of_stream ~length:body_length stream)
    | None -> None
  in
  let open Util.Result.Syntax in
  let* response = Client.request ~sw ~config ~meth ~headers ?body env uri in
  handle_response ~cli ~sw ~stdout:(Eio.Stdenv.stdout env) response

let rec request_many ~sw ~cli ~config env urls =
  let { default_proto; _ } = cli in
  match urls with
  | [] ->
    (* Never happens, cmdliner guarantees we get a non-empty list. *)
    assert false
  | [ x ] ->
    let uri = uri_of_string ~scheme:default_proto x in
    let r = request ~sw ~cli ~config env uri in
    (match r with
    | Ok () -> `Ok ()
    | Error e -> `Error (false, Error.to_string e))
  | x :: xs ->
    let uri = uri_of_string ~scheme:default_proto x in
    let _r = request ~sw ~cli ~config env uri in
    request_many ~sw ~cli ~config env xs

let log_level_of_list ~silent = function
  | [] -> if silent then None else Some Logs.Warning
  | [ _ ] -> Some Info
  | _ -> Some Debug

let piaf_config_of_cli
    { follow_redirects
    ; max_redirects
    ; max_http_version
    ; h2c_upgrade
    ; http2_prior_knowledge
    ; cacert
    ; capath
    ; insecure
    ; min_tls_version
    ; max_tls_version
    ; connect_timeout
    ; data
    ; head
    ; _
    }
  =
  match data, head with
  | Some _, true ->
    let msg =
      "You can only select one HTTP request method! You asked for both POST \
       (-d / --data) and HEAD (-I / --head)"
    in
    Logs.warn (fun m -> m "%s" msg);
    Error msg
  | _, _ ->
    Ok
      { Config.default with
        follow_redirects
      ; max_redirects
      ; max_http_version
      ; h2c_upgrade
      ; http2_prior_knowledge
      ; cacert
      ; capath
      ; allow_insecure = insecure
      ; min_tls_version
      ; max_tls_version
      ; connect_timeout
      ; body_buffer_size = 0x10000
      }

let main ({ log_level; urls; _ } as cli) =
  setup_log log_level;
  if (not Ansi.dumb) && Ansi.isatty
  then Fmt.set_style_renderer (Format.get_std_formatter ()) `Ansi_tty;
  match piaf_config_of_cli cli with
  | Error msg -> `Error (false, msg)
  | Ok config ->
    Eio_main.run (fun env ->
        Eio.Switch.run (fun sw -> request_many ~sw ~cli ~config env urls))

(* --resolve <host:port:address[,address]...> Resolve the host+port to this address
 * --retry <num>   Retry request if transient problems occur
 * --retry-connrefused Retry on connection refused (use with --retry)
 * --retry-delay <seconds> Wait time between retries
 * --retry-max-time <seconds> Retry only within this period
 *)
module CLI = struct
  let request =
    let request_conv =
      let parse meth = Ok (Method.of_string meth) in
      let print = Method.pp_hum in
      Arg.conv ~docv:"method" (parse, print)
    in
    let doc = "Specify request method to use" in
    let docv = "method" in
    Arg.(
      value & opt (some request_conv) None & info [ "X"; "request" ] ~doc ~docv)

  let cacert =
    let cert_conv =
      let parse s = Ok (Cert.Filepath s) in
      Arg.conv ~docv:"method" (parse, Cert.pp)
    in
    let doc = "CA certificate to verify peer against" in
    let docv = "file" in
    Arg.(value & opt (some cert_conv) None & info [ "cacert" ] ~doc ~docv)

  let capath =
    let doc = "CA directory to verify peer against" in
    let docv = "dir" in
    Arg.(value & opt (some string) None & info [ "capath" ] ~doc ~docv)

  let cert =
    let doc = "Client certificate file path" in
    let docv = "file" in
    Arg.(value & opt (some string) None & info [ "E"; "cert" ] ~doc ~docv)

  let compressed =
    let doc = "Request compressed response" in
    Arg.(value & flag & info [ "compressed" ] ~doc)

  let connect_timeout =
    let doc = "Maximum time allowed for connection" in
    let docv = "seconds" in
    Arg.(value & opt float 30. & info [ "connect-timeout" ] ~doc ~docv)

  let data =
    let doc = "HTTP POST data" in
    let docv = "data" in
    Arg.(value & opt (some string) None & info [ "d"; "data" ] ~doc ~docv)

  let insecure =
    let doc = "Allow insecure server connections when using SSL" in
    Arg.(value & flag & info [ "k"; "insecure" ] ~doc)

  let key =
    let doc = "Client certificate private key" in
    let docv = "file" in
    Arg.(value & opt (some string) None & info [ "key" ] ~doc ~docv)

  let default_proto =
    let doc = "Use $(docv) for any URL missing a scheme (without `://`)" in
    let docv = "protocol" in
    Arg.(value & opt string "http" & info [ "proto-default" ] ~doc ~docv)

  let head =
    let doc = "Show document info only" in
    Arg.(value & flag & info [ "I"; "head" ] ~doc)

  let headers =
    let header_conv =
      let parse header =
        match String.split_on_char ':' header with
        | [] -> Error (`Msg "Header can't be the empty string")
        | [ x ] ->
          Error
            (`Msg (Format.asprintf "Expecting `name: value` string, got: %s" x))
        | name :: values ->
          let value = Util.String.trim_left (String.concat ":" values) in
          Ok (name, value)
      in
      let print = format_header in
      Arg.conv ~docv:"method" (parse, print)
    in
    let doc = "Pass custom header(s) to server" in
    let docv = "header" in
    Arg.(value & opt_all header_conv [] & info [ "H"; "header" ] ~doc ~docv)

  let include_ =
    let doc = "Include protocol response headers in the output" in
    Arg.(value & flag & info [ "i"; "include" ] ~doc)

  let follow_redirects =
    let doc = "Follow redirects" in
    Arg.(value & flag & info [ "L"; "location" ] ~doc)

  let max_redirects =
    let doc = "Max number of redirects to follow" in
    let docv = "int" in
    Arg.(
      value
      & opt int Config.default.max_redirects
      & info [ "max-redirs" ] ~doc ~docv)

  let use_http_1_0 =
    let doc = "Use HTTP 1.0" in
    Arg.(value & flag & info [ "0"; "http1.0" ] ~doc)

  let use_http_1_1 =
    let doc = "Use HTTP 1.1" in
    Arg.(value & flag & info [ "http1.1" ] ~doc)

  let use_http_2 =
    let doc = "Use HTTP 2" in
    Arg.(value & flag & info [ "http2" ] ~doc)

  let http_2_prior_knowledge =
    let doc = "Use HTTP 2 without HTTP/1.1 Upgrade" in
    Arg.(value & flag & info [ "http2-prior-knowledge" ] ~doc)

  let referer =
    let doc = "Referrer URL" in
    let docv = "URL" in
    Arg.(value & opt (some string) None & info [ "e"; "referer" ] ~doc ~docv)

  let silent =
    let doc = "Silent mode" in
    Arg.(value & flag & info [ "s"; "silent" ] ~doc)

  let verbose =
    let doc = "Verbosity (use multiple times to increase)" in
    Arg.(value & flag_all & info [ "v"; "verbose" ] ~doc)

  let sslv3 =
    let doc = "Use SSLv3" in
    Arg.(value & flag & info [ "3"; "sslv3" ] ~doc)

  let tlsv1 =
    let doc = "Use TLSv1.0 or greater" in
    Arg.(value & flag & info [ "1"; "tlsv1" ] ~doc)

  let tlsv1_0 =
    let doc = "Use TLSv1.0 or greater" in
    Arg.(value & flag & info [ "tlsv1.0" ] ~doc)

  let tlsv1_1 =
    let doc = "Use TLSv1.1 or greater" in
    Arg.(value & flag & info [ "tlsv1.1" ] ~doc)

  let tlsv1_2 =
    let doc = "Use TLSv1.2 or greater" in
    Arg.(value & flag & info [ "tlsv1.2" ] ~doc)

  let tlsv1_3 =
    let doc = "Use TLSv1.3 or greater" in
    Arg.(value & flag & info [ "tlsv1.3" ] ~doc)

  let tls_max_version =
    let tls_conv =
      let parse s =
        match Versions.TLS.of_string s with
        | Ok v -> Ok v
        | Error msg -> Error (`Msg msg)
      in
      let print = Versions.TLS.pp_hum in
      Arg.conv ~docv:"" (parse, print)
    in
    let doc = "Set maximum allowed TLS version" in
    let docv = "version" in
    Arg.(
      value & opt tls_conv Versions.TLS.TLSv1_3 & info [ "tls-max" ] ~doc ~docv)

  let upload_file =
    let doc = "Transfer local $(docv) to destination" in
    let docv = "file" in
    Arg.(
      value & opt (some string) None & info [ "T"; "upload-file" ] ~doc ~docv)

  let user_agent =
    let doc = "Send User-Agent $(docv) to server" in
    let docv = "name" in
    Arg.(
      value
      & opt string "carl/0.0.0-experimental"
      & info [ "A"; "user-agent" ] ~doc ~docv)

  let user =
    let doc = "Server user and password" in
    let docv = "user:password" in
    Arg.(value & opt (some string) None & info [ "u"; "user" ] ~doc ~docv)

  let oauth2_bearer =
    let doc = "OAuth 2 Bearer Token" in
    let docv = "token" in
    Arg.(value & opt (some string) None & info [ "oauth2-bearer" ] ~doc ~docv)

  let output =
    let output_conv =
      let parse s =
        let output = Channel s in
        Ok output
      in
      let print formatter output =
        let s = match output with Stdout -> "stdout" | Channel f -> f in
        Format.fprintf formatter "%s" s
      in
      Arg.conv ~docv:"method" (parse, print)
    in
    let doc = "Write to file instead of stdout" in
    let docv = "file" in
    Arg.(value & opt output_conv Stdout & info [ "o"; "output" ] ~doc ~docv)

  let urls =
    let docv = "URLs" in
    Arg.(non_empty & pos_all string [] & info [] ~docv)

  let parse
      cacert
      capath
      cert
      compressed
      connect_timeout
      data
      default_proto
      head
      headers
      include_
      insecure
      key
      follow_redirects
      max_redirects
      request
      use_http_1_0
      use_http_1_1
      use_http_2
      http2_prior_knowledge
      referer
      sslv3
      tlsv1
      tlsv1_0
      tlsv1_1
      tlsv1_2
      tlsv1_3
      max_tls_version
      upload_file
      user_agent
      user
      oauth2_bearer
      silent
      verbose
      output
      urls
    =
    { follow_redirects
    ; max_redirects
    ; data =
        (match data, upload_file with
        | None, None -> None
        | Some data, _ ->
          (* `-d` takes precedence over `-T` *)
          Some (Data data)
        | None, Some filename -> Some (File filename))
    ; meth =
        (match head, request, data with
        | true, None, None -> `HEAD
        | _, None, Some _ -> `POST
        | _, None, None -> `GET
        | _, Some meth, _ -> meth)
    ; log_level = log_level_of_list ~silent verbose
    ; urls
    ; default_proto
    ; head
    ; headers
    ; include_
    ; max_http_version =
        (match use_http_2, use_http_1_1, use_http_1_0 with
        | true, _, _ | false, false, false ->
          (* Default to the highest supported if no override specified. *)
          Versions.HTTP.HTTP_2
        | false, true, _ -> HTTP_1_1
        | false, false, true -> HTTP_1_0)
    ; h2c_upgrade = use_http_2
    ; http2_prior_knowledge
    ; cacert
    ; capath
    ; clientcert =
        (match cert, key with
        | Some cert, Some key -> Some (Cert.Filepath cert, Cert.Filepath key)
        | _ -> None)
    ; min_tls_version =
        (* select the _maximum_ min version *)
        Versions.TLS.(
          match tlsv1_3, tlsv1_2, tlsv1_1, tlsv1_0, tlsv1, sslv3 with
          | true, _, _, _, _, _ -> TLSv1_3
          | _, true, _, _, _, _ -> TLSv1_2
          | _, _, true, _, _, _ -> TLSv1_1
          | _, _, _, true, _, _ -> TLSv1_0
          | _, _, _, _, true, _ -> TLSv1_0
          | _, _, _, _, _, true -> SSLv3
          | _, _, _, _, _, _ -> TLSv1_0)
    ; max_tls_version
    ; insecure
    ; user_agent
    ; compressed
    ; connect_timeout
    ; referer
    ; user
    ; oauth2_bearer
    ; output
    }

  let default_cmd =
    Term.(
      const parse
      $ cacert
      $ capath
      $ cert
      $ compressed
      $ connect_timeout
      $ data
      $ default_proto
      $ head
      $ headers
      $ include_
      $ insecure
      $ key
      $ follow_redirects
      $ max_redirects
      $ request
      $ use_http_1_0
      $ use_http_1_1
      $ use_http_2
      $ http_2_prior_knowledge
      $ referer
      $ sslv3
      $ tlsv1
      $ tlsv1_0
      $ tlsv1_1
      $ tlsv1_2
      $ tlsv1_3
      $ tls_max_version
      $ upload_file
      $ user_agent
      $ user
      $ oauth2_bearer
      $ silent
      $ verbose
      $ output
      $ urls)

  let cmd =
    let doc = "Like curl, for caml" in
    let info = Cmd.info "carl" ~version:"todo" ~doc in
    Cmd.v info Term.(ret (const main $ default_cmd))
end

let () = exit (Cmd.eval CLI.cmd)
