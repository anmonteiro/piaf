type t =
  { filename : string
  ; content_type : string
  ; body : Body.t
  }

let body ?(max_chunk_size = 0x100000) (request : Request.t) =
  let open Lwt.Syntax in
  (* TODO(anmonteiro): validate max content-length from a config, etc. *)
  let content_type = Headers.get request.headers "content-type" in
  match content_type with
  | Some content_type ->
    (* TODO(anmonteiro): might wanna check `or_error` too. *)
    let stream, _or_error = Body.to_stream request.body in
    let waiter, wakener = Lwt.wait () in
    let emit name stream = Lwt.wakeup_later wakener (name, stream) in
    let* multipart_result =
      Multipart.parse_multipart_form ~content_type ~max_chunk_size ~emit stream
    in
    (match multipart_result with
    | Error e ->
      Lwt.return_error e
    | Ok multipart_headers ->
      let* name, stream = waiter in
      let parsed_name, hs =
        List.hd (Multipart.result_fields multipart_headers)
      in
      assert (Option.get name = parsed_name);
      let content_type = Multipart.content_type hs in
      let content_disposition = Option.get (Multipart.content_disposition hs) in
      (* From RFC7578§4.2:
          *
       *   For form data that represents the content of a file, a
       *   name for the file SHOULD be supplied as well, by using a "filename"
       *   parameter of the Content-Disposition header field. *)
      let filename =
        Option.get
          (Multipart_form.Content_disposition.filename content_disposition)
      in
      Lwt.return_ok
        { filename =
            (* From RFC2183§2.3:
             *   The receiving MUA SHOULD NOT respect any directory path
             *   information that may seem to be present in the filename
             *   parameter. The filename should be treated as a terminal
             *   component only. *)
            Filename.basename filename
        ; content_type =
            Format.asprintf "%a" Multipart.Pp.pp_content_type content_type
        ; body = Body.of_stream stream
        })
  | None ->
    Lwt.return_error (`Msg "Missing `content-type` header for multipart upload")
