open! Core

let get_fname ~tag ~day ~year ~extension =
  let day_string = if day < 10 then "0" ^ Int.to_string day else Int.to_string day in
  [%string "lib/solutions/problem_%{Int.to_string year}_%{day_string}%{tag}%{extension}"]
;;

let make_mli ~day ~year ~tag =
  let fname = get_fname ~day ~year ~tag ~extension:".mli" in
  let mli_string =
    {| open! Core
val part1 : string -> string
val part2 : string -> string |}
  in
  match Sys_unix.file_exists_exn fname with
  | true -> ()
  | false -> Out_channel.write_all fname ~data:mli_string
;;

let make_ml ~day ~year ~tag =
  let fname = get_fname ~day ~year ~tag ~extension:".ml" in
  let ml_string =
    {| open! Core

let part1 _ = raise_s [%message "unimplemented"]
let part2 _ = raise_s [%message "unimplemented"] |}
  in
  match Sys_unix.file_exists_exn fname with
  | true -> ()
  | false -> Out_channel.write_all fname ~data:ml_string
;;

let make_day ~day ~year ~tag =
  make_mli ~day ~year ~tag;
  make_ml ~day ~year ~tag
;;
