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
module Option = struct
  include Option

  let ( let+ ) option f = Option.map f option

  let ( let* ) = Option.bind

  let ( and* ) o1 o2 =
    match o1, o2 with Some x, Some y -> Some (x, y) | _ -> None
end

module Result = struct
  include Result

  let ( let+ ) result f = map f result

  let ( let* ) = bind

  let ( and* ) r1 r2 =
    match r1, r2 with
    | Ok x, Ok y ->
      Ok (x, y)
    | Ok _, Error e | Error e, Ok _ | Error e, Error _ ->
      Error e
end

module Lwt_result = struct
  include Lwt_result

  let error e = Lwt.map (fun e -> Error e) e

  module Syntax = struct
    let ( let+ ) x f = map f x

    let ( let* ) = bind
  end
end

module Bindings = struct
  (* use `let*` / `let+` for Lwt. These are the ones we're going to end up
   * using the most *)
  include Lwt.Syntax

  (* Option *)
  open Option

  let ( let*? ) = ( let* )

  let ( let+? ) = ( let+ )

  let ( and*? ) = ( and* )

  (* Result *)

  open Result

  let ( let*! ) = ( let* )

  let ( let+! ) = ( let+ )

  let ( and*! ) = ( and* )

  (* Lwt_result *)
  open Lwt_result.Syntax

  let ( let**! ) = ( let* )

  let ( let++! ) = ( let+ )

  let ( and**! ) = ( and* )
end
