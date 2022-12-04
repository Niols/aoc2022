(** {1 Day 0 -- Sample} *)

open Ext

(** {2 Parsing} *)

let section_assignment_pairs : ((int * int) * (int * int)) list =
  let comma = Str.regexp "," in
  let dash = Str.regexp "-" in
  let range = Read.(pair ~sep:dash int int) in
  Read.(lines_until_empty (pair ~sep:comma range range))

(** {2 Part 1} *)

(** Whether the interval [needle] is included in the interval [haystack]. *)
let interval_inclusion ~needle:(n1, n2) ~haystack:(h1, h2) =
  h1 <= n1 && n2 <= h2

(** Whether one of the two intervals is included in the other. *)
let any_interval_inclusion interval1 interval2 =
  interval_inclusion ~needle:interval1 ~haystack:interval2
  || interval_inclusion ~needle:interval2 ~haystack:interval1

let () =
  section_assignment_pairs
  |> List.map (uncurry any_interval_inclusion)
  |> List.filter Fun.id
  |> List.length
  |> pf "%d@."

(** {2 Part 2} *)
