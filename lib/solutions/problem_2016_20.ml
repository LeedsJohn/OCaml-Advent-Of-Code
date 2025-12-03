open! Core
open! Helpers

let of_string s =
  let nums =
    String.split_lines s
    |> List.map ~f:(fun line ->
      match Parse.line_numbers line with
      | [ a; b ] -> a, b
      | _ -> raise_s [%message "wtf (parsing)"])
    |> List.sort ~compare:(fun (n1, _) (n2, _) -> Int.compare n1 n2)
  in
  let rec aux acc = function
    | [] -> List.rev acc
    | (low, high) :: tl ->
      (match acc with
       | [] -> aux [ low, high ] tl
       | (prev_low, prev_high) :: tl2 ->
         if low + 1 <= prev_high
         then aux ((prev_low, Int.max high prev_high) :: tl2) tl
         else aux ((low, high) :: acc) tl)
  in
  aux [] nums
;;

let part1 s =
  let rec aux = function
    | [ _ ] | [] -> raise_s [%message "wtf"]
    | (_, h1) :: (l2, h2) :: tl -> if h1 + 1 < l2 then h1 + 1 else aux ((l2, h2) :: tl)
  in
  aux (of_string s) |> Int.to_string
;;

let part2 s =
  let nums = of_string s in
  let total = List.sum (module Int) nums ~f:(fun (l, h) -> h - l + 1) in
  Int.shift_left 1 32 - total |> Int.to_string
;;
