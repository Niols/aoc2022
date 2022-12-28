(** {1 Day 5 -- Supply Stacks} *)

open Next

(** {2 Parsing} *)

type crate = char [@@deriving show]

type stacks = crate list list [@@deriving show]

type move = {
  number : int ;
  from : int ;
  to_ : int ;
}
[@@deriving show]

let stacks =
  let lines : crate option list list =
    (* This is a bit tricky to parse. We will gather all the lines first. *)
    GRead.(lines_until_empty string)
    (* We drop the last line. *)
    |> List.bd
    (* For each line, we will find all the characters in a position equal to 1
       modulo 4. This will give us a list of crates per line. *)
    |> List.map
      (fun line ->
         String.to_seqi line
         |> Seq.filter_map (fun (i, c) ->
             if i mod 4 = 1 then Some c else None)
         |> Seq.map (function ' ' -> None | c -> Some c)
         |> List.of_seq
      )
      (* We still have to somewhat transpose this list of lists. *)
  in
  let rec somewhat_transpose : crate option list list -> crate list list =
    function
    | [] -> assert false
    | [crates] -> crates |> List.map Option.get |> List.map List.singleton
    | crates :: lines ->
      somewhat_add_on_top crates @@ somewhat_transpose lines
  and somewhat_add_on_top (crates : crate option list) (stacks : crate list list) =
    match crates, stacks with
    | [], stacks -> stacks
    | None::crates, stack::stacks -> stack :: somewhat_add_on_top crates stacks
    | (Some crate)::crates, stack::stacks -> (crate::stack) :: somewhat_add_on_top crates stacks
    | _ -> assert false
  in
  somewhat_transpose lines

let moves =
  GRead.(lines_until_empty (tuple6 string int string int string int))
  |> List.map (function
      | ("move", number, "from", from, "to", to_) ->
        { number; from = from-1; to_=to_-1 }
      | _ -> assert false)

(** {2 Part 1} *)

open List

let apply_move custom_append move stacks =
  let stacks =
    let crates = stacks |> flip nth move.from |> take move.number in
    stacks
    |> update_nth move.from (drop move.number)
    |> update_nth move.to_ (custom_append crates)
  in
  stacks

let stack_tops = map hd

let () =
  fold_left (flip @@ apply_move rev_append) stacks moves
  |> stack_tops
  |> iter (pf "%c");
  pf "@."

(** {2 Part 2} *)

let () =
  fold_left (flip @@ apply_move append) stacks moves
  |> stack_tops
  |> iter (pf "%c");
  pf "@."
