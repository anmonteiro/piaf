open Monads
module IOVec = H2.IOVec

type length =
  [ `Fixed of Int64.t
  | `Chunked
  | `Error of [ `Bad_request | `Bad_gateway | `Internal_server_error ]
  | `Unknown
  | `Close_delimited
  ]

type t =
  { length : length
  ; stream : Bigstringaf.t IOVec.t Lwt_stream.t
  }

let create ~body_length stream = { length = body_length; stream }

let to_string t =
  let open Lwt.Syntax in
  let len =
    match t.length with
    | `Fixed n ->
      Int64.to_int n
    | _ ->
      (* TODO: use some config? *)
      100
  in
  let result_buffer = Buffer.create len in
  let+ () =
    Lwt_stream.iter
      (fun { IOVec.buffer; off; len } ->
        let bytes = Bytes.create len in
        Bigstringaf.blit_to_bytes buffer ~src_off:off ~dst_off:0 ~len bytes;
        Buffer.add_bytes result_buffer bytes)
      t.stream
  in
  Buffer.contents result_buffer

let to_string_stream t =
  Lwt_stream.map
    (fun { IOVec.buffer; off; len } -> Bigstringaf.substring buffer ~off ~len)
    t.stream

let drain t = Lwt_stream.junk_while (fun _ -> true) t.stream
