open! Core

let get_fname ~day ~year ~extension =
  let day_string =
    if day < 10 then "0" ^ Int.to_string day else Int.to_string day
  in
  [%string
    "lib/solutions/problem_%{Int.to_string year}_%{day_string}%{extension}"]

let make_mli ~day ~year =
  let fname = get_fname ~day ~year ~extension:".mli" in
  let mli_string =
    {| open! Core
val part1 : string -> string Or_error.t
val part2 : string -> string Or_error.t |}
  in
  match Sys_unix.file_exists_exn fname with
  | true -> ()
  | false -> Out_channel.write_all fname ~data:mli_string

let make_ml ~day ~year =
  let fname = get_fname ~day ~year ~extension:".ml" in
  let ml_string =
    {| open! Core

let part1 _ = Error (Error.of_string "Unimplemented")
let part2 _ = Error (Error.of_string "Unimplemented") |}
  in
  match Sys_unix.file_exists_exn fname with
  | true -> ()
  | false -> Out_channel.write_all fname ~data:ml_string

let make_day ~day ~year =
  make_mli ~day ~year;
  make_ml ~day ~year
