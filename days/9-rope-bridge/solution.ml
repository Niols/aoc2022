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

type state = {
  head_x : int ;
  head_y : int ;
  tail_x : int ;
  tail_y : int ;
}

let initial_state = {
  head_x = 0 ;
  head_y = 0 ;
  tail_x = 0 ;
  tail_y = 0 ;
}

let move_head s = function
  | Right -> { s with head_x = s.head_x + 1 }
  | Up    -> { s with head_y = s.head_y + 1 }
  | Left  -> { s with head_x = s.head_x - 1 }
  | Down  -> { s with head_y = s.head_y - 1 }

let move_tail s =
  let should_move =
    abs (s.head_x - s.tail_x) > 1
    || abs (s.head_y - s.tail_y) > 1
  in
  if not should_move then
    (
      s
    )
  else
    (
      let tail_x =
        if s.head_x > s.tail_x then
          s.tail_x + 1
        else if s.head_x < s.tail_x then
          s.tail_x - 1
        else
          s.tail_x
      in
      let tail_y =
        if s.head_y > s.tail_y then
          s.tail_y + 1
        else if s.head_y < s.tail_y then
          s.tail_y - 1
        else
          s.tail_y
      in
      { s with tail_x; tail_y }
    )

(** A set of positions. *)
module PSet = Set.Make (struct type t = int * int let compare = compare end)

let initial_pset = PSet.singleton (0, 0)

let move_and_gobble (s, pset) dir =
  let s = move_tail @@ move_head s dir in
  (s, PSet.add (s.tail_x, s.tail_y) pset)

let initial = (initial_state, initial_pset)

let _ =
  directions
  |> List.fold_left move_and_gobble initial
  |> snd
  |> PSet.cardinal
  |> pf "%d@."

(** {2 Part 2} *)
