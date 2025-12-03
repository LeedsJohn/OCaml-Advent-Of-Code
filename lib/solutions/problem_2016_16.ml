open! Core

(* there's a way to do this by examining the patterns but why bother when it only takes
   a few seconds to run for part 2 *)

let dragon a =
  let b =
    String.rev a
    |> String.map ~f:(function
      | '0' -> '1'
      | _ -> '0')
  in
  a ^ "0" ^ b
;;

let rec dragon_until_length s n =
  if String.length s >= n then String.slice s 0 n else dragon_until_length (dragon s) n
;;

let checksum s =
  let rec step acc = function
    | [ _ ] | [] -> List.rev acc |> String.of_list
    | a :: b :: tl ->
      if Char.equal a b then step ('1' :: acc) tl else step ('0' :: acc) tl
  in
  let rec aux s =
    if String.length s % 2 = 1 then s else aux (step [] (String.to_list s))
  in
  aux (step [] (String.to_list s))
;;

let part1 s = dragon_until_length s 272 |> checksum
let part2 s = dragon_until_length s 35651584 |> checksum
