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

(** The state of the rope: head first followed by its tails. *)
type rope = position list

let initial_rope size = List.init size @@ Fun.const initial_position

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

let move_rope rope dir =
  match rope with
  | [] -> invalid_arg "move_rope"
  | head :: tails ->
    let head = move head dir in
    let rev_rope =
      List.fold_left
        (fun rev_rope tail ->
           let tail = follow ~lead:(List.hd rev_rope) tail in
           tail :: rev_rope)
        [head]
        tails
    in
    List.rev rev_rope

(** A set of positions. *)
module PSet = Set.Make (struct type t = position let compare = compare end)

let initial_pset = PSet.singleton (0, 0)

let move_and_gobble (rope, pset) dir =
  let rope = move_rope rope dir in
  let last = List.ft rope in
  (rope, PSet.add last pset)

let initial = (initial_rope 2, initial_pset)

let _ =
  directions
  |> List.fold_left move_and_gobble initial
  |> snd
  |> PSet.cardinal
  |> pf "%d@."

(** {2 Part 2} *)

let initial = (initial_rope 10, initial_pset)

let _ =
  directions
  |> List.fold_left move_and_gobble initial
  |> snd
  |> PSet.cardinal
  |> pf "%d@."
