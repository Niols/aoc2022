type 'a cast = string -> 'a

let int s =
  match int_of_string_opt s with
  | None -> failwith "ExtRead.int"
  | Some n -> n

let bit s = int s = 1

let float s =
  match float_of_string_opt s with
  | None -> failwith "ExtRead.float"
  | Some f -> f

let char x =
  if String.length x <> 1 then
    failwith "ExtRead.char";
  x.[0]

let string s = s

let no_space_string s =
  if String.index_opt s ' ' <> None then
    failwith "ExtRead.no_space_string";
  s

let list c s =
  if s = "" then []
  else String.split_on_char ' ' s |> List.map c

let non_empty_list c s =
  match list c s with
  | [] -> failwith "ExtRead.non_empty_list"
  | l -> l

let array c s = list c s |> Array.of_list
let non_empty_array c s = non_empty_list c s |> Array.of_list

let tuple2g c1 c2 s =
  match ExtString.split_on_char_n 2 ' ' s with
  | [v1; v2] -> (c1 v1, c2 v2)
  | _ -> assert false

let tuple2 c s = tuple2g c c s
let pairg = tuple2g
let pair = tuple2

let tuple3g c1 c2 c3 s =
  match ExtString.split_on_char_n 3 ' ' s with
  | [v1; v2; v3] -> (c1 v1, c2 v2, c3 v3)
  | _ -> assert false

let tuple3 c s = tuple3g c c c s
let tripleg = tuple3g
let triple = tuple3

let tuple4g c1 c2 c3 c4 s =
  match ExtString.split_on_char_n 4 ' ' s with
  | [v1; v2; v3; v4] -> (c1 v1, c2 v2, c3 v3, c4 v4)
  | _ -> assert false

let tuple4 c s = tuple4g c c c c s

let tuple5g c1 c2 c3 c4 c5 s =
  match ExtString.split_on_char_n 5 ' ' s with
  | [v1; v2; v3; v4; v5] -> (c1 v1, c2 v2, c3 v3, c4 v4, c5 v5)
  | _ -> assert false

let tuple5 c s = tuple5g c c c c c s

let of_string cast s = cast s

let line_of_chan ichan cast = input_line ichan |> string cast
let line cast = line_of_chan stdin cast

let lines_of_chan_until_empty ichan cast =
  let rec aux acc =
    match input_line ichan with
    | exception End_of_file when acc = [] -> raise End_of_file
    | exception End_of_file -> List.rev acc
    | "" -> List.rev acc
    | line -> aux @@ (line |> string cast) :: acc
  in
  aux []
let lines_until_empty cast = lines_of_chan_until_empty stdin cast

let%test_module _ = (module struct
  let test cast string expected =
    let result = try Ok (of_string cast string) with exn -> Error exn in
    match result, expected with
    | Ok result, Ok expected when result = expected -> true
    | Error result, Error expected when expected result -> true
    | _ -> false

  (* let any _ = true *)
  let failure = function Failure _ -> true | _ -> false
  (* let invalid_arg = function Invalid_argument _ -> true | _ -> false *)

  let%test _ = test int "7" (Ok 7)
  let%test _ = test int "L" (Error failure)

  let%test _ = test bit "1" (Ok true)
  let%test _ = test bit "0" (Ok false)
  let%test _ = test bit "T" (Error failure)

  let%test _ = test float "34.2" (Ok 34.2)
  let%test _ = test float "TRUE" (Error failure)

  let%test _ = test char "Y" (Ok 'Y')
  let%test _ = test char "YO" (Error failure)

  let%test _ = test string "Bonjour" (Ok "Bonjour")

  let%test _ = test no_space_string "Bonjour" (Ok "Bonjour")
  let%test _ = test no_space_string "Bon jour" (Error failure)

  let%test _ = test (list int) "1 2 7" (Ok [1; 2; 7])
  let%test _ = test (list int) "1 L 7" (Error failure)
  let%test _ = test (list int) "" (Ok [])
  let%test _ = test (non_empty_list int) "" (Error failure)

  let%test _ = test (array int) "1 2 7" (Ok [|1; 2; 7|])
  let%test _ = test (array int) "1 L 7" (Error failure)
  let%test _ = test (array int) "" (Ok [||])
  let%test _ = test (non_empty_array int) "" (Error failure)

  let%test _ = test (pair int) "7 8" (Ok (7, 8))
  let%test _ = test (pair int) "7 L" (Error failure)
  let%test _ = test (pair int) "7" (Error failure)
  let%test _ = test (pair int) "7 8 9" (Error failure)

  let%test _ = test (pairg int float) "7 34.2" (Ok (7, 34.2))
  let%test _ = test (tuple2 int) "8 9" (Ok (8, 9))
  let%test _ = test (tuple2g int float) "7 34.2" (Ok (7, 34.2))

  let%test _ = test (tuple3 int) "7 8 9" (Ok (7, 8, 9))
  let%test _ = test (tuple3 int) "7 L 9" (Error failure)
  let%test _ = test (tuple3 int) "7 8" (Error failure)
  let%test _ = test (tuple3 int) "7 8 9 10" (Error failure)

  let%test _ = test (tuple3g int float string) "7 8 9" (Ok (7, 8., "9"))
  let%test _ = test (tuple3g int float string) "7 8" (Error failure)
  let%test _ = test (tuple3g int float string) "7 8 9 10" (Ok (7, 8., "9 10"))
  let%test _ = test (tuple3g int float no_space_string) "7 8 9 10" (Error failure)

  let%test _ = test (tuple4 int) "7 8 9 10" (Ok (7, 8, 9, 10))
  let%test _ = test (tuple4g int float char string) "7 8 9 10" (Ok (7, 8., '9', "10"))
  let%test _ = test (tuple4g int float char string) "7 8 9 10 11" (Ok (7, 8., '9', "10 11"))
  let%test _ = test (tuple4g int float char no_space_string) "7 8 9 10 11" (Error failure)

  let%test _ = test (tuple5 int) "7 8 9 10 11" (Ok (7, 8, 9, 10, 11))
  let%test _ = test (tuple5g int float bit char string) "7 8 1 9 10" (Ok (7, 8., true, '9', "10"))
  let%test _ = test (tuple5g int float bit char string) "7 8 0 9 10 11" (Ok (7, 8., false, '9', "10 11"))
  let%test _ = test (tuple5g int float bit char no_space_string) "7 8 0 9 10 11" (Error failure)

  let%test _ = test (pairg int (list string)) "7 8 9 10" (Ok (7, ["8"; "9"; "10"]))
  let%test _ = test (pairg int (list string)) "7" (Error failure)

  let%test _ = test (pairg int (array string)) "7 8 9 10" (Ok (7, [|"8"; "9"; "10"|]))
  let%test _ = test (pairg int (array string)) "7" (Error failure)
end)
