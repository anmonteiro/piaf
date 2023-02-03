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
open Monads.Bindings
open Util
module Version = Httpaf.Version

let src = Logs.Src.create "piaf.connection" ~doc:"Piaf Connection module"

module Log = (val Logs.src_log src : Logs.LOG)

let resolve_host env ~config ~port hostname : (_, [> Error.client ]) result =
  let clock = Eio.Stdenv.clock env in
  let network = Eio.Stdenv.net env in
  match
    Eio.Time.with_timeout_exn clock config.Config.connect_timeout (fun () ->
        Eio.Net.getaddrinfo_stream
          ~service:(string_of_int port)
          network
          hostname)
  with
  | [] ->
    Error
      (`Connect_error (Format.asprintf "Can't resolve hostname: %s" hostname))
  | xs ->
    (match config.Config.prefer_ip_version with
    | `Both ->
      let order_v4v6 = Eio.Net.Ipaddr.fold ~v4:(fun _ -> -1) ~v6:(fun _ -> 1) in
      Ok
        (* Sort IPv4 ahead of IPv6 for compatibility. *)
        (List.sort
           (fun a1 a2 ->
             match a1, a2 with
             | `Unix s1, `Unix s2 -> String.compare s1 s2
             | `Tcp (ip1, _), `Tcp (ip2, _) ->
               compare (order_v4v6 ip1) (order_v4v6 ip2)
             | `Unix _, `Tcp _ -> 1
             | `Tcp _, `Unix _ -> -1)
           xs)
    | `V4 ->
      Ok
        (List.filter
           (function
             | `Tcp (ip, _) ->
               Eio.Net.Ipaddr.fold ~v4:(fun _ -> true) ~v6:(fun _ -> false) ip
             | `Unix _ -> true)
           xs)
    | `V6 ->
      Ok
        (List.filter
           (function
             | `Tcp (ip, _) ->
               Eio.Net.Ipaddr.fold ~v4:(fun _ -> false) ~v6:(fun _ -> true) ip
             | `Unix _ -> true)
           xs))
  | exception Eio.Time.Timeout ->
    Error
      (`Connect_error
        (Format.asprintf "Timed out resolving hostname: %s" hostname))

module Info = struct
  (* This represents information that changes from connection to connection,
   * i.e. if one of these parameters changes between redirects we need to
   * establish a new connection. *)
  type t =
    { port : int
    ; scheme : Scheme.t
    ; addresses : Eio.Net.Sockaddr.stream list
    ; uri : Uri.t
    ; host : string
    }

  (* Only need the address and port to know whether the endpoint is the same or
   * not. *)
  let equal c1 c2 =
    c1.port = c2.port
    && (* At least one can match *)
    List.exists
      (fun a1 ->
        List.exists
          (fun a2 ->
            match a1, a2 with
            | `Tcp (addr1, _), `Tcp (addr2, _) ->
              String.equal (Obj.magic addr1 : string) (Obj.magic addr2 : string)
            | `Unix addr1, `Unix addr2 -> String.equal addr1 addr2
            | _ -> false)
          c2.addresses)
      c1.addresses

  (* Use this shortcut to avoid resolving the new address. Not 100% correct
   * because different hosts may point to the same address. *)
  let equal_without_resolving c1 c2 =
    c1.port = c2.port
    && c1.scheme = c2.scheme
    && Uri.host_exn c1.uri = Uri.host_exn c2.uri

  let pp_address fmt = function
    | `Tcp (addr, port) ->
      Format.fprintf fmt "%a:%d" Eio.Net.Ipaddr.pp addr port
    | `Unix addr -> Format.fprintf fmt "%s" addr

  let pp_hum fmt { addresses; host; _ } =
    let address = List.hd addresses in
    Format.fprintf fmt "%s (%a)" host pp_address address

  let infer_port ~scheme uri =
    match Uri.port uri with
    (* if a port is given, use it. *)
    | Some port -> port
    (* Otherwise, infer from the scheme. *)
    | None -> Scheme.to_port scheme

  let of_uri env ~config uri =
    let uri = Uri.canonicalize uri in
    match Uri.host uri, Scheme.of_uri uri with
    | Some host, Ok scheme ->
      let port = infer_port ~scheme uri in
      let+! addresses = resolve_host env ~config ~port host in
      { scheme; uri; host; port; addresses }
    | None, _ ->
      Error (`Msg (Format.asprintf "Missing host part for: %a" Uri.pp_hum uri))
    | _, (Error _ as error) -> error
end

let connect ~sw ~clock ~network ~config conn_info =
  let { Info.addresses; _ } = conn_info in
  (* TODO: try addresses in e.g. a round robin fashion? *)
  let address = List.hd addresses in
  Log.debug (fun m -> m "Trying connection to %a" Info.pp_hum conn_info);
  match
    Eio.Time.with_timeout_exn clock config.Config.connect_timeout (fun () ->
        Eio.Net.connect ~sw network address)
  with
  | sock -> Ok sock
  | exception exn ->
    (match exn with
    | Eio.Io (Eio.Net.E (Connection_failure code), _) ->
      let status =
        match code with
        | Eio.Net.Refused _ -> "connection refused"
        | No_matching_addresses -> "no matching addresses"
        | Timeout -> "timeout"
      in
      Error
        (`Connect_error
          (Format.asprintf
             "Failed connecting to %a: %s"
             Info.pp_hum
             conn_info
             status))
    | Eio.Io (Eio.Exn.X (Eio_unix.Unix_error (code, _, _)), _) ->
      Error
        (`Connect_error
          (Format.asprintf
             "Failed connecting to %a: %s"
             Info.pp_hum
             conn_info
             (Unix.error_message code)))
    | Eio.Time.Timeout ->
      Error
        (`Connect_error
          (Format.asprintf
             "Failed connecting to %a: connection timeout"
             Info.pp_hum
             conn_info))
    | exn ->
      Error
        (`Connect_error
          (Format.asprintf
             "FIXME: unhandled connection error (%s)"
             (Printexc.to_string exn))))

type t =
  | Conn :
      { impl : (module Http_intf.HTTPCommon with type Client.t = 'a)
      ; connection : 'a
      ; fd : < Eio.Net.stream_socket ; Eio.Flow.close >
      ; mutable info : Info.t
      ; mutable uri : Uri.t
            (* The connection URI. Request entrypoints connect here. Mutable so
               that we remember permanent redirects. *)
      ; mutable persistent : bool
      ; runtime : Gluten_eio.Client.t
      ; connection_error_received : Error.client Promise.t
      ; version : Versions.HTTP.t (* HTTP version that this connection speaks *)
      }
      -> t
