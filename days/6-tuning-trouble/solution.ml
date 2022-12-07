(** {1 Day 6 -- Tuning Trouble} *)

open Ext

(** {2 Parsing} *)

let stream = Read.(line string)
let length = String.length stream

(** {2 Part 1} *)

exception Found of int

let sop_marker_position =
  try
    for pos = 4 to length do
      let char4 = [ stream.[pos - 4]; stream.[pos - 3]; stream.[pos - 2]; stream.[pos - 1] ] in
      let n = List.(length @@ sort_uniq Char.compare char4) in
      if n = 4 then raise (Found pos)
    done;
    assert false
  with
    Found pos -> pos

let () = pf "%d@." sop_marker_position

(** {2 Part 2} *)
