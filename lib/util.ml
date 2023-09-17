(*----------------------------------------------------------------------------
 * Copyright (c) 2019, António Nuno Monteiro
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

module Uri = struct
  include Uri

  let host_exn uri =
    match Uri.host uri with
    | Some host -> host
    | None -> raise (Failure "host_exn")

  let parse_with_base_uri ~scheme ~uri location =
    let location_uri = Uri.of_string location in
    let new_uri =
      match Uri.host location_uri with
      | Some _ -> location_uri
      | None ->
        (* relative URI, replace the path and query on the old URI. *)
        Uri.resolve (Scheme.to_string scheme) uri location_uri
    in
    Uri.canonicalize new_uri
end

module Backtrace = struct
  let pp_hum formatter raw_backtrace =
    let backtrace_slots =
      Option.map
        (fun slots ->
           Array.to_list slots
           |> List.mapi (fun i slot -> Printexc.Slot.format i slot))
        (Printexc.backtrace_slots raw_backtrace)
    in
    let format_backtrace_slot formatter slot =
      match slot with
      | Some slot -> Format.fprintf formatter "@[<h 0>%s@]" slot
      | None -> ()
    in
    (Format.pp_print_list
       ~pp_sep:(fun f () -> Format.fprintf f "@;")
       format_backtrace_slot)
      formatter
      (Option.value ~default:[] backtrace_slots)
end
