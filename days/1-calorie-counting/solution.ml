(** {1 Day 1 -- Calorie Counting} *)

open Next

(** {2 Parsing}

    We represent the elfs as an [int list list]: each elf corresponds to a list
    of calories/integers and we have a list of elves. We represent the calories
    as an [int list]: each elf is associated to their total number of
    calories. *)

let elfs : int list list =
  (* We use {!Read.lines_until_empty} to get a batch of integers from standard
     input and to stop on the first empty line. We use {!List.init_until} to
     repeat this process until we encounter an {!End_of_file}. *)
  List.init_until @@ fun _ ->
  try Some GRead.(lines_until_empty int) with End_of_file -> None

let calories : int list = List.map (List.fold_left (+) 0) elfs

(** {2 Part 1}

    We want the maximum number in the list {!calories}. *)

let () = pf "%d@." (List.fold_left max 0 calories)

(** {2 Part 2}

    We want the sum of the three maximal numbers in {!calories}. *)

let max3 =
  (* NOTE: The inversion of [a] and [b] in the argument of {!List.sort} is on
     purpose. *)
  match List.sort (fun a b -> Int.compare b a) calories with
  | top1 :: top2 :: top3 :: _ -> pf "%d@." (top1 + top2 + top3)
  | _ -> epf "NOT ENOUGH ELVES!@."; exit 7
