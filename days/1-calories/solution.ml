open Ext

(* Parsing *)

let elfs : int list list =
  List.init_until @@ fun _ ->
  try Some Read.(lines_until_empty int) with End_of_file -> None

let calories : int list = List.map (List.fold_left (+) 0) elfs

(* Part 1 *)

let max : int = List.fold_left max 0 calories

let () = pf "Maximimum amount of calories: %d.@." max

(* Part 2 *)

let max3 =
  match List.sort (fun a b -> Int.compare b a) calories with
  | top1 :: top2 :: top3 :: _ ->
    pf "Amount of calories in top 3 is: %d.@." (top1 + top2 + top3)
  | _ ->
    epf "NOT ENOUGH ELVES!@."
