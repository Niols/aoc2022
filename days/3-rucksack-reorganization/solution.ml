(** {1 Day 3 -- Rucksack Reorganization} *)

open Next

(** {2 Parsing} *)

type item = char
type compartment = item list
type rucksack = compartment * compartment

let contents : rucksack list =
  GRead.(lines_until_empty string)
  |> List.map (fun line -> String.fold_right List.cons line [])
  |> List.map (fun line ->
      let length = List.length line in
      assert (length mod 2 = 0);
      let l = length / 2 in
      (List.sub line 0 l, List.sub line l l))

(** {2 Part 1} *)

let intersect l1 l2 =
  List.filter (fun x1 -> List.exists (fun x2 -> compare x1 x2 = 0) l2) l1

let priority item =
  if 'a' <= item && item <= 'z' then
    Char.code item - Char.code 'a' + 1
  else if 'A' <= item && item <= 'Z' then
    Char.code item - Char.code 'A' + 27
  else
    invalid_arg "priority"

let () =
  contents
  |> List.map (fun (l1, l2) -> List.hd @@ intersect l1 l2)
  |> List.map priority
  |> List.fold_left (+) 0
  |> pf "%d@."

(** {2 Part 2} *)

let contents_by_group : (rucksack * rucksack * rucksack) list =
  let rec aux acc = function
    | [] -> List.rev acc
    | r1 :: r2 :: r3 :: contents -> aux ((r1, r2, r3) :: acc) contents
    | _ -> failwith "contents not a list of triples"
  in
  aux [] contents

let () =
  contents_by_group
  |> List.map (fun ((c11, c12), (c21, c22), (c31, c32)) ->
      List.hd @@ intersect (c11 @ c12) @@ intersect (c21 @ c22) (c31 @ c32))
  |> List.map priority
  |> List.fold_left (+) 0
  |> pf "%d@."
