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

open Monads
open Util
module Version = Httpaf.Version

let resolve_host ~port hostname =
  let open Lwt.Syntax in
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

module Connection_info = struct
  (* This represents information that changes from connection to connection,
   * i.e. if one of these parameters changes between redirects we need to
   * establish a new connection. *)
  type t =
    { port : int
    ; scheme : Scheme.t
    ; addresses : Unix.sockaddr list
    ; uri : Uri.t
          (** Current URI the connection is connected to. A redirect could've
              made the remote endpoint change.

              Guaranteed to have a host part. *)
    ; host : string  (** `Uri.host t.uri` but makes our life easier. *)
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
            (* Note: this is slightly wrong if we allow both UNIX and Internet
             * domain sockets but for now we filter by TCP sockets that we can
             * connect to. *)
            match a1, a2 with
            | Unix.ADDR_INET (addr1, _), Unix.ADDR_INET (addr2, _) ->
              String.equal
                (Unix.string_of_inet_addr addr1)
                (Unix.string_of_inet_addr addr2)
            | ADDR_UNIX addr1, ADDR_UNIX addr2 ->
              String.equal addr1 addr2
            | _ ->
              false)
          c2.addresses)
      c1.addresses

  (* Use this shortcut to avoid resolving the new address. Not 100% correct
   * because different hosts may point to the same address. *)
  let equal_without_resolving c1 c2 =
    c1.port = c2.port
    && c1.scheme = c2.scheme
    && Uri.host_exn c1.uri = Uri.host_exn c2.uri

  let infer_port ~scheme uri =
    match Uri.port uri, scheme with
    (* if a port is given, use it. *)
    | Some port, _ ->
      port
    (* Otherwise, infer from the scheme. *)
    | None, Scheme.HTTPS ->
      443
    | None, HTTP ->
      80

  let of_uri uri =
    let open Lwt_result.Syntax in
    let uri = Uri.canonicalize uri in
    match Uri.host uri, Scheme.of_uri uri with
    | Some host, Ok scheme ->
      let port = infer_port ~scheme uri in
      let+ addresses = resolve_host ~port host in
      { scheme; uri; host; port; addresses }
    | None, _ ->
      Lwt_result.fail
        (`Msg (Format.asprintf "Missing host part for: %a" Uri.pp_hum uri))
    | _, (Error _ as error) ->
      Lwt.return error

  let pp_address fmt = function
    | Unix.ADDR_INET (addr, port) ->
      Format.fprintf fmt "%s:%d" (Unix.string_of_inet_addr addr) port
    | ADDR_UNIX addr ->
      Format.fprintf fmt "%s" addr

  let pp_hum fmt { addresses; host; _ } =
    let address = List.hd addresses in
    Format.fprintf fmt "%s (%a)" host pp_address address
end

type t =
  | Conn :
      { impl :
          (module Http_intf.HTTPCommon
             with type Client.t = 'a
              and type Body.Reader.t = 'b)
      ; handle : 'a
      ; runtime : Scheme.Runtime.t
      ; connection_error_received : Error.client Lwt.t
      ; version : Version.t  (** HTTP version that this connection speaks *)
      }
      -> t
