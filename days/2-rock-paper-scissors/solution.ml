open Ext

(** {1 Parsing}

    We represent the shapes as a data type with three constructors. We represent
    a guide as a list of pairs of shapes. *)

type shape = Rock | Paper | Scissors

let shape_of_string = function
  | "A" | "X" -> Rock
  | "B" | "Y" -> Paper
  | "C" | "Z" -> Scissors
  | _ -> invalid_arg "move_of_abc"

let guide : (shape * shape) list =
  Read.(lines_until_empty @@ pair @@ cast shape_of_string)

(* {1 Part 1}

   We want the score of the guide according the the given rules. *)

let shape_score = function
  | Rock -> 1
  | Paper -> 2
  | Scissors -> 3

type outcome = Won | Lost | Draw

let outcome_score = function
  | Lost -> 0
  | Draw -> 3
  | Won -> 6

let outcome ~you ~other =
  match (you, other) with
  | (Rock, Rock) | (Paper, Paper) | (Scissors, Scissors) -> Draw
  | (Rock, Scissors) | (Paper, Rock) | (Scissors, Paper) -> Won
  | (Rock, Paper) | (Paper, Scissors) | (Scissors, Rock) -> Lost

let () =
  guide
  |> List.map (fun (other, you) ->
      shape_score you
      + outcome_score (outcome ~you ~other))
  |> List.fold_left (+) 0
  |> pf "%d@."
