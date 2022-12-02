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

(** {2 Part 1}

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

(** {2 Part 2} *)

(* In fact, X Y Z was not Rock/Paper/Scissors, but an outcome. We fix the given
   guide. It's a bit ugly but at least it follows the way the instructions were
   given. *)

let fix_shape_into_outcome = function
  | Rock -> Lost
  | Paper -> Draw
  | Scissors -> Won

let outcome_guide : (shape * outcome) list =
  List.map (fun (s1, s2) -> (s1, fix_shape_into_outcome s2)) guide

let shape ~other ~outcome =
  match (other, outcome) with
  | (Rock, Draw) | (Paper, Lost) | (Scissors, Won) -> Rock
  | (Rock, Won) | (Paper, Draw) | (Scissors, Lost) -> Paper
  | (Rock, Lost) | (Paper, Won) | (Scissors, Draw) -> Scissors

let () =
  outcome_guide
  |> List.map (fun (other, outcome) ->
      shape_score (shape ~other ~outcome)
      + outcome_score outcome)
  |> List.fold_left (+) 0
  |> pf "%d@."
