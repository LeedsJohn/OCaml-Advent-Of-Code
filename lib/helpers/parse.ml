open! Core

let take_int_no_negative s =
  let rec aux acc s =
    if String.length s = 0 || not (Char.is_digit (String.get s 0))
    then acc, s
    else aux (acc ^ String.of_char (String.get s 0)) (String.drop_prefix s 1)
  in
  aux "" s
;;

let take_int ?(default = 0) s =
  if String.length s > 0
  then (
    let negative = Char.equal (String.get s 0) '-' in
    let num_str, new_s =
      if negative
      then take_int_no_negative (String.drop_prefix s 1)
      else take_int_no_negative s
    in
    if String.length num_str = 0
    then default, s
    else if negative
    then -1 * Int.of_string num_str, new_s
    else Int.of_string num_str, new_s)
  else default, s
;;

let take_next_int ?(default = 0) s =
  match String.findi s ~f:(fun _ c -> Char.is_digit c || Char.equal c '-') with
  | None -> default, ""
  | Some (i, _) -> take_int (String.slice s i 0)
;;

let line_numbers s =
  let rec aux acc s =
    if String.equal s ""
    then List.rev acc
    else if
      Char.is_digit (String.get s 0)
      || (String.length s > 1
          && Char.equal '-' (String.get s 0)
          && Char.is_digit (String.get s 1))
    then (
      let num, s = take_int s in
      aux (num :: acc) (String.drop_prefix s 1))
    else aux acc (String.drop_prefix s 1)
  in
  aux [] s
;;
