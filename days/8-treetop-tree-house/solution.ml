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

let trees_top x y = List.init x (fun i -> (i, y)) |> List.rev
let trees_bottom x y = List.init (length_x - x - 1) (fun i -> (x + 1 + i, y))
let trees_left x y = List.init y (fun j -> (x, j)) |> List.rev
let trees_right x y = List.init (length_y - y - 1) (fun j -> (x, y + 1 + j))

let is_tree_visible x y =
  let top = trees_top x y in
  let bottom = trees_bottom x y in
  let left = trees_left x y in
  let right = trees_right x y in
  let visible_from =
    List.for_all (fun (i, j) -> tree_sizes.(i).(j) < tree_sizes.(x).(y))
  in
  visible_from top
  || visible_from bottom
  || visible_from left
  || visible_from right

let all_positions =
  List.init length_x (fun x -> List.init length_y (fun y -> (x, y)))
  |> List.flatten

let () =
  all_positions
  |> List.filter (uncurry is_tree_visible)
  |> List.length
  |> pf "%d@."

(** {2 Part 2} *)

let scenic_score x y =
  let top = trees_top x y in
  let bottom = trees_bottom x y in
  let left = trees_left x y in
  let right = trees_right x y in
  let viewable_trees_from direction =
    direction
    |> List.prefix_until_inclusive (fun (i, j) -> tree_sizes.(i).(j) >= tree_sizes.(x).(y))
    |> List.length
  in
  viewable_trees_from top
  * viewable_trees_from bottom
  * viewable_trees_from left
  * viewable_trees_from right

let () =
  all_positions
  |> List.map (uncurry scenic_score)
  |> List.fold_left max 0
  |> pf "%d@."
