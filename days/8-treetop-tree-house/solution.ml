(** {1 Day 8 -- Treetop Tree House} *)

open Ext

(** {2 Parsing} *)

(** All the tree sizes; starting from the top left corner. *)
let tree_sizes : int array array =
  Read.(lines_until_empty string)
  |> List.map String.to_seq
  |> List.map (Seq.map int_of_char)
  |> List.map (Array.of_seq)
  |> Array.of_list

let length_x = Array.length tree_sizes
let length_y = Array.length tree_sizes.(0)

(** {2 Part 1} *)

type int_int_list = (int * int) list [@@deriving show]

let is_tree_visible x y =
  let top = List.init x (fun i -> (i, y)) in
  let bottom = List.init (length_x - x - 1) (fun i -> (x + 1 + i, y)) in
  let left = List.init y (fun j -> (x, j)) in
  let right = List.init (length_y - y - 1) (fun j -> (x, y + 1 + j)) in
  let visible_from =
    List.for_all (fun (i, j) -> tree_sizes.(i).(j) < tree_sizes.(x).(y))
  in
  visible_from top
  || visible_from bottom
  || visible_from left
  || visible_from right

let () =
  List.init length_x (fun x -> List.init length_y (fun y -> (x, y)))
  |> List.flatten
  |> List.filter (uncurry is_tree_visible)
  |> List.length
  |> pf "%d@."

(** {2 Part 2} *)
