open! Core

(* part 1: 1:47, part 2: 4:08 *)

let f s =
  let n = Int.of_string s in
  (n / 3) - 2
;;

let rec fuel2 n =
  if n <= 0
  then 0
  else (
    let num = (n / 3) - 2 in
    Int.max num 0 + fuel2 num)
;;

let part1 s =
  List.fold (String.split_lines s) ~init:0 ~f:(fun acc line -> acc + f line)
  |> Int.to_string
;;

let part2 s =
  List.fold (String.split_lines s) ~init:0 ~f:(fun acc line ->
    acc + fuel2 (Int.of_string line))
  |> Int.to_string
;;
