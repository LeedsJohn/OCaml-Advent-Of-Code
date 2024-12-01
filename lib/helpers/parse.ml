open! Core

let take_int_exn s =
  let rec aux acc s =
    if String.length s = 0 || not (Char.is_digit (String.get s 0)) then
      (Int.of_string acc, s)
    else aux (acc ^ String.of_char (String.get s 0)) (String.drop_prefix s 1)
  in
  if Char.(String.get s 0 = '-') then aux "-" (String.drop_prefix s 1)
  else aux "" s
