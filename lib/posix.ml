(*----------------------------------------------------------------------------
 * Copyright (c) 2022, António Nuno Monteiro
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

let src = Logs.Src.create "piaf.posix" ~doc:"Piaf POSIX module"

module Log = (val Logs.src_log src : Logs.LOG)

let sendfile
    (type a)
    (module Bodyw : Body.BODY with type Writer.t = a)
    ~src_fd
    ~dst_fd
    raw_write_body
  =
  (* Flush everything to the wire before calling `sendfile`, as we're gonna
     bypass the http/af runtime and write bytes to the file descriptor
     directly. *)
  let sent_ret = Sendfile.sendfile ~src:src_fd dst_fd in
  match sent_ret with
  | Ok sent ->
    (* NOTE(anmonteiro): we don't need to
     * `Gluten.Server.report_write_result` here given that we put
     * bytes in the file descriptor off-band. `report_write_result`
     * just tracks the internal Faraday buffers. *)
    Log.debug (fun m -> m "`sendfile` wrote %d bytes successfully" sent);
    Ok ()
  | Error e ->
    Bodyw.Writer.close raw_write_body;
    Error (Unix.Unix_error (e, "sendfile", "") (* TODO(anmonteiro): log err *))
