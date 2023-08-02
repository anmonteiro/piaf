(*----------------------------------------------------------------------------
 * Copyright (c) 2020, António Nuno Monteiro
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

include H2.IOVec

let make buffer ~off ~len = { buffer; off; len }

let of_string s ~off ~len =
  { buffer = Bigstringaf.of_string s ~off ~len; off; len }

let of_bytes bytes ~off ~len =
  let buffer = Bigstringaf.create len in
  Bigstringaf.blit_from_bytes bytes ~src_off:off buffer ~dst_off:0 ~len;
  { buffer; off; len }

let concat = function
  | [] -> make Bigstringaf.empty ~off:0 ~len:0
  | [ iovec ] -> iovec
  | iovecs ->
    let length = lengthv iovecs in
    let result_buffer = Bigstringaf.create length in
    let aux acc_off { buffer; off; len } =
      Bigstringaf.unsafe_blit
        buffer
        ~src_off:off
        result_buffer
        ~dst_off:acc_off
        ~len;
      acc_off + len
    in
    let _len : int = List.fold_left aux 0 iovecs in
    { buffer = result_buffer; off = 0; len = length }
