(** {1 Day 6 -- Tuning Trouble} *)

open Ext

(** {2 Parsing} *)

let stream = Read.(line string)
let length = String.length stream

(** {2 Part 1} *)

exception Found of int

let marker_position nb_chars =
  try
    for pos = nb_chars to length do
      let chars = List.init nb_chars (fun i -> stream.[pos - (i + 1)]) in
      let n = List.(length @@ sort_uniq Char.compare chars) in
      if n = nb_chars then raise (Found pos)
    done;
    assert false
  with
    Found pos -> pos

let () = pf "%d@." (marker_position 4)

(** {2 Part 2} *)

let () = pf "%d@." (marker_position 14)
