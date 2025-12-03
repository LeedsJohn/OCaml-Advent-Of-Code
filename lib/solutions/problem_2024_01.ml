open! Core
open! Helpers

let nums s =
  String.split_lines s
  |> List.map ~f:(fun line ->
    let l, s = Parse.take_int line in
    let s = String.filter s ~f:Char.is_digit in
    let r, _ = Parse.take_int s in
    l, r)
;;

let part1 s =
  let stuff = nums s in
  let left = List.map stuff ~f:fst in
  let right = List.map stuff ~f:snd in
  let res =
    List.fold
      (List.zip_exn
         (List.sort left ~compare:Int.compare)
         (List.sort right ~compare:Int.compare))
      ~init:0
      ~f:(fun acc (l, r) -> acc + Int.abs (r - l))
  in
  Int.to_string res
;;

let part2 s =
  let stuff = nums s in
  let left = List.map stuff ~f:fst in
  let right = List.map stuff ~f:snd in
  let res =
    List.fold left ~init:0 ~f:(fun acc n -> acc + (n * List.count right ~f:(Int.equal n)))
  in
  Int.to_string res
;;
