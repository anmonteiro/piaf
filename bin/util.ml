module String = struct
  (* https://github.com/ocaml/ocaml/blob/34e6149044b54feeceaf24fb0126a477a288e058/stdlib/string.ml#L93-L95 *)
  let is_space = function
    | ' ' | '\012' | '\n' | '\r' | '\t' ->
      true
    | _ ->
      false

  let rec find f s i =
    if i > String.length s - 1 then
      -1
    else if f (String.unsafe_get s i) then
      i
    else
      find f s (i + 1)

  let trim_left s =
    if s = "" then
      s
    else
      let idx = find (fun c -> not (is_space c)) s 0 in
      if idx = -1 then
        s
      else
        Bytes.unsafe_to_string
          (Bytes.sub (Bytes.unsafe_of_string s) idx (String.length s - idx))
end
