open! Core

let get_marker s i =
  let x = String.substr_index_exn ~pos:i s ~pattern:"x" in
  let end_spot = String.substr_index_exn ~pos:x s ~pattern:")" in
  ( Int.of_string (String.slice s (i + 1) x)
  , Int.of_string (String.slice s (x + 1) end_spot)
  , end_spot + 1 )
;;

let get_length s =
  let s = String.filter s ~f:(fun c -> not (Char.is_whitespace c)) in
  let rec aux acc i =
    if i >= String.length s
    then acc
    else if Char.equal (String.get s i) '('
    then (
      let num_chars, repeats, i = get_marker s i in
      aux (acc + (num_chars * repeats)) (i + num_chars))
    else aux (acc + 1) (i + 1)
  in
  aux 0 0
;;

let get_length2 s =
  let s = String.filter s ~f:(fun c -> not (Char.is_whitespace c)) in
  let rec aux s =
    if String.equal s ""
    then 0
    else if Char.( <> ) (String.get s 0) '('
    then 1 + aux (String.drop_prefix s 1)
    else (
      let num_chars, repeats, i = get_marker s 0 in
      (repeats * aux (String.slice s i (i + num_chars)))
      + aux (String.slice s (i + num_chars) 0))
  in
  aux s
;;

let part1 s = get_length s |> Int.to_string
let part2 s = get_length2 s |> Int.to_string
