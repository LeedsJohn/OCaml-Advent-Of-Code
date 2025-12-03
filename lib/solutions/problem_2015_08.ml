open! Core

let count_characters s =
  let rec aux i = function
    | [] -> i
    | [ _ ] -> i + 1
    | a :: b :: c :: d :: tl ->
      if Char.(a = '\\' && b = 'x' && is_hex_digit c && is_hex_digit d)
      then aux (i + 1) tl
      else if Char.(a = '\\' && (b = '"' || b = '\\'))
      then aux (i + 1) (c :: d :: tl)
      else aux (i + 1) (b :: c :: d :: tl)
    | a :: b :: tl ->
      if Char.(a = '\\' && (b = '"' || b = '\\'))
      then aux (i + 1) tl
      else aux (i + 1) (b :: tl)
  in
  aux (-2) (String.to_list s)
;;

let encoded_length s =
  String.count s ~f:(fun c -> Char.(c = '\\' || c = '"')) + String.length s + 2
;;

let part1 s =
  let lines = String.split_lines s in
  let total_chars = List.sum (module Int) lines ~f:String.length in
  let other_chars = List.sum (module Int) lines ~f:count_characters in
  print_s [%message (total_chars : int) (other_chars : int)];
  total_chars - other_chars |> Int.to_string
;;

let part2 s =
  let lines = String.split_lines s in
  let total_chars = List.sum (module Int) lines ~f:String.length in
  let other_chars = List.sum (module Int) lines ~f:encoded_length in
  print_s [%message (total_chars : int) (other_chars : int)];
  other_chars - total_chars |> Int.to_string
;;
