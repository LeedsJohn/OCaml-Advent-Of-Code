open! Core
open! Helpers

(* too lazy to install a regex library but that would make this cleaner i think *)

let get_numbers_in_parens s =
  let n1, s = if Char.is_digit (String.get s 0) then Parse.take_int s else 0, s in
  let n1, s =
    if String.length s > 0 && Char.equal (String.get s 0) ','
    then n1, String.drop_prefix s 1
    else 0, s
  in
  let n2, s = if Char.is_digit (String.get s 0) then Parse.take_int s else 0, s in
  let n2 = if String.length s > 0 && Char.equal (String.get s 0) ')' then n2 else 0 in
  n1 * n2
;;

let part1 s =
  let rec aux acc s =
    if String.length s < 8
    then acc
    else (
      match String.prefix s 4 with
      | "mul(" ->
        aux (acc + get_numbers_in_parens (String.slice s 4 0)) (String.drop_prefix s 4)
      | _ -> aux acc (String.drop_prefix s 1))
  in
  aux 0 s |> Int.to_string
;;

let part2 s =
  let instruction_type s =
    let open String in
    if prefix s 4 = "mul("
    then `Mul
    else if prefix s 4 = "do()"
    then `Do
    else if prefix s 7 = "don't()"
    then `Don't
    else `John
  in
  let rec aux acc enabled s =
    if String.length s < 8
    then acc
    else (
      match instruction_type s with
      | `Mul ->
        if not enabled
        then aux acc enabled (String.drop_prefix s 4)
        else
          aux
            (acc + get_numbers_in_parens (String.slice s 4 0))
            true
            (String.drop_prefix s 4)
      | `Do -> aux acc true (String.drop_prefix s 1)
      | `Don't -> aux acc false (String.drop_prefix s 1)
      | `John -> aux acc enabled (String.drop_prefix s 1))
  in
  aux 0 true s |> Int.to_string
;;
