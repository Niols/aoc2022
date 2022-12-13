(** {1 Day 7 -- No Space Left On Device} *)

open Ext

module Path = struct
  type t = string list

  let cd (path : t) : string -> t = function
    | "/" -> []
    | ".." -> List.bd path
    | fname -> path @ [fname]
end

(** {2 Parsing} *)

module Filesystem = struct
  type t = entry list
  and entry = string * file
  and file = Directory of entry list | RegularFile of int
  [@@deriving show {with_path=false}]

  type zipper = {
    entries : entry list;
    ancestors : ancestor list
  }
  and ancestor = {
    before : entry list ;
    filename : string ;
    after : entry list
  }

  let zip (filesystem : t) =
    { entries = filesystem; ancestors = [] }

  let head zipper = zipper.entries
  let with_head entries zipper = { zipper with entries }

  let up zipper =
    match zipper.ancestors with
    | [] -> None
    | ancestor::ancestors ->
      Some {
        ancestors ;
        entries =
          (List.rev ancestor.before)
          @ [(ancestor.filename, Directory zipper.entries)]
          @ ancestor.after
      }

  let rec root zipper =
    match up zipper with
    | None -> zipper
    | Some zipper -> root zipper

  let unzip zipper = head (root zipper)

  let down zipper target_filename =
    let rec down_assoc before = function
      | [] -> None
      | ((filename, _) as entry) :: entries when filename <> target_filename ->
        down_assoc (entry :: before) entries
      | (_, RegularFile _) :: _ -> None
      | (filename, Directory entries) :: after ->
        Some {
          entries ;
          ancestors = { before ; filename ; after } :: zipper.ancestors
        }
    in
    down_assoc [] zipper.entries

  let cd zipper = function
    | "/" -> Some (root zipper)
    | ".." -> up zipper
    | filename -> down zipper filename
end

let rec apply_lines zipper =
  match Read.(line string) with
  | exception End_of_file -> zipper
  | line when String.sub line 0 2 = "$ " ->
    (
      match String.sub line 2 2 with
      | "cd" -> apply_lines @@ Option.get @@ Filesystem.cd zipper @@ String.(sub line 5 (length line - 5))
      | "ls" -> apply_lines zipper
      | _ -> assert false
    )
  | line ->
    (
      let (dir_or_size, filename) = Read.(of_string (pair string string)) line in
      let entry =
        (filename,
         if dir_or_size = "dir"
         then Filesystem.Directory []
         else Filesystem.RegularFile (ios dir_or_size))
      in
      let head = Filesystem.head zipper in
      assert (not @@ List.mem_assoc filename head);
      apply_lines @@ Filesystem.with_head (entry :: head) zipper
    )

let filesystem = Filesystem.unzip @@ apply_lines @@ Filesystem.zip []

(** {2 Part 1} *)

(** Returns the size of the directory as well as the list of sizes of
    directories contained within. *)
let rec size_and_dir_sizes entries =
  let (size, dir_sizes) =
    entries
    |> List.map (fun (_, file) -> file_size_and_dir_sizes file)
    |> List.fold_left
      (fun (total_size, total_dir_sizes) (size, dir_sizes) ->
         (total_size + size, List.rev_append dir_sizes total_dir_sizes))
      (0, [])
  in
  (size, size :: dir_sizes)

(** Returns the size of the file as well as the list of sizes of directories
    contained within. *)
and file_size_and_dir_sizes = function
  | Filesystem.RegularFile size -> (size, [])
  | Filesystem.Directory entries -> size_and_dir_sizes entries

(** Returns the size of a directory. *)
let size entries = fst @@ size_and_dir_sizes entries

(** Returns the list of sizes of directories in the given directory, including
    the directory itself. *)
let dir_sizes entries = snd @@ size_and_dir_sizes entries

let () =
  filesystem
  |> dir_sizes
  |> List.filter (fun size -> size <= 100_000)
  |> List.fold_left (+) 0
  |> pf "%d@."

(** {2 Part 2} *)

let total_space = 70_000_000
let target_unused = 30_000_000

let current_unused = total_space - size filesystem
let missing_unused = target_unused - current_unused

let () =
  filesystem
  |> dir_sizes
  |> List.filter (fun size -> size >= missing_unused)
  |> List.sort Int.compare
  |> List.hd
  |> pf "%d@."
