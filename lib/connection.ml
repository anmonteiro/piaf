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

open Monads.Bindings
open Util
module Version = Httpaf.Version

let src = Logs.Src.create "piaf.connection" ~doc:"Piaf Connection module"

module Log = (val Logs.src_log src : Logs.LOG)

let resolve_host ~port hostname =
  let+ addresses =
    Lwt_unix.getaddrinfo
      hostname
      (string_of_int port)
      (* https://www.iana.org/assignments/protocol-numbers/protocol-numbers.xhtml *)
      Unix.[ AI_CANONNAME; AI_PROTOCOL 6; AI_FAMILY PF_INET ]
  in
  match addresses with
  | [] ->
    let msg = Format.asprintf "Can't resolve hostname: %s" hostname in
    Error (`Connect_error msg)
  | xs ->
    (* TODO: add resolved canonical hostname *)
    Ok (List.map (fun { Unix.ai_addr; _ } -> ai_addr) xs)

module Connect_info = struct
  type t =
    { port : int
    ; scheme : Scheme.t
    ; uri : Uri.t
          (** Current URI the connection is connected to. A redirect could've
              made the remote endpoint change.

              Guaranteed to have a host part. *)
    ; host : string  (** `Uri.host t.uri` but makes our life easier. *)
    }

  let equal t1 t2 = t1.port = t2.port && t1.host = t2.host

  let infer_port ~scheme uri =
    match Uri.port uri with
    (* if a port is given, use it. *)
    | Some port -> port
    (* Otherwise, infer from the scheme. *)
    | None -> Scheme.to_port scheme

  let of_uri uri =
    let uri = Uri.canonicalize uri in
    match Uri.host uri, Scheme.of_uri uri with
    | Some host, Ok scheme ->
      let port = infer_port ~scheme uri in
      Ok { scheme; uri; host; port }
    | None, _ ->
      Error (`Msg (Format.asprintf "Missing host part for: %a" Uri.pp_hum uri))
    | _, (Error _ as error) -> error
end

module Connected_info = struct
  (* This represents information that changes from connection to connection,
   * i.e. if one of these parameters changes between redirects we need to
   * establish a new connection. *)
  type t =
    { port : int
    ; scheme : Scheme.t
    ; uri : Uri.t
    ; host : string
    ; addr : Ipaddr.t
    }

  let to_connect_info { port; scheme; uri; host; _ } =
    { Connect_info.port; scheme; uri; host }

  let merge (t : t) ({ uri; host; port; scheme; _ } : Connect_info.t) =
    { t with uri; host; port; scheme }

  let pp_hum fmt { host; addr; port; _ } =
    Format.fprintf fmt "%s (%a:%d)" host Ipaddr.pp addr port
end

let connect ~config ~port hostname =
  let happy_eyeballs =
    Happy_eyeballs.create
      ~connect_timeout:(Duration.of_ms config.Config.connect_timeout)
      (* ~resolve_timeout *)
      (* ~resolve_retries *)
      (Mtime_clock.elapsed_ns ())
  in
  let he = Happy_eyeballs_lwt.create ~happy_eyeballs () in
  Log.debug (fun m -> m "Trying connection to %s%d" hostname port);
  let+ connect_result = Happy_eyeballs_lwt.connect he hostname [ port ] in
  match connect_result with
  | Ok info -> Ok info
  | Error (`Msg s) ->
    (* | Lwt_unix.Timeout -> let msg = Format.asprintf "Connection timed out
       after %.0f milliseconds" (config.connect_timeout *. 1000.) in Log.err
       (fun m -> m "%s" msg); Lwt_result.fail (`Connect_error msg) |
       Unix.Unix_error (ECONNREFUSED, _, _) -> Lwt_result.fail (`Connect_error
       (Format.asprintf "Failed connecting to %a: connection refused"
       Connection_info.pp_hum conn_info)) *)
    Error (`Connect_error (Format.asprintf "Connection failed: %s" s))

type t =
  | Conn :
      { impl :
          (module Http_intf.HTTPCommon
             with type Client.t = 'a
              and type Body.Reader.t = 'b)
      ; handle : 'a
      ; mutable conn_info : Connected_info.t
      ; runtime : Scheme.Runtime.t
      ; connection_error_received : Error.client Lwt.t
      ; version : Version.t  (** HTTP version that this connection speaks *)
      }
      -> t
