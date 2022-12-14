(** {1 Day 0 -- Sample} *)

open Ext

(** {2 Parsing} *)

type direction = Right | Up | Left | Down

let direction = Read.cast @@ function
  | "R" -> Right
  | "U" -> Up
  | "L" -> Left
  | "D" -> Down
  | _ -> invalid_arg "direction_of_letter"

let directions =
  Read.(lines_until_empty (pair direction int))
  |> List.map (fun (dir, nb) -> List.init nb (Fun.const dir))
  |> List.flatten

(** {2 Part 1} *)

type position = int * int

let initial_position = (0, 0)

(** The state of the rope: head first followed by its tail. *)
type state = position list

let initial_state size = List.init size @@ Fun.const initial_position

let move (x, y) = function
  | Right -> (x + 1, y)
  | Up    -> (x, y + 1)
  | Left  -> (x - 1, y)
  | Down  -> (x, y - 1)

let follow ~lead:(lead_x, lead_y) (x, y) =
  let should_move =
    abs (lead_x - x) > 1
    || abs (lead_y - y) > 1
  in
  if not should_move then
    (
      (x, y)
    )
  else
    (
      (
        if lead_x > x then
          x + 1
        else if lead_x < x then
          x - 1
        else
          x
      ),
      (
        if lead_y > y then
          y + 1
        else if lead_y < y then
          y - 1
        else
          y
      )
    )

(** A set of positions. *)
module PSet = Set.Make (struct type t = position let compare = compare end)

let initial_pset = PSet.singleton (0, 0)

let move_and_gobble (head_pos, tail_pos, pset) dir =
  let head_pos = move head_pos dir in
  let tail_pos = follow ~lead:head_pos tail_pos in
  (head_pos, tail_pos, PSet.add tail_pos pset)

let initial = (initial_position, initial_position, initial_pset)

let _ =
  directions
  |> List.fold_left move_and_gobble initial
  |> (fun (_head_pos, _tail_pos, pset) -> pset)
  |> PSet.cardinal
  |> pf "%d@."

(** {2 Part 2} *)
