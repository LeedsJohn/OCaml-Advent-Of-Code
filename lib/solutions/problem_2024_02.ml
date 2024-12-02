open! Core

let parse s =
  String.split_lines s
  |> List.map ~f:(fun line ->
         String.split line ~on:' ' |> List.map ~f:Int.of_string)

let rec is_increasing = function
  | a :: b :: tl ->
      if Int.between (b - a) ~low:1 ~high:3 then is_increasing (b :: tl)
      else false
  | _ -> true

let is_decreasing l = is_increasing (List.rev l)

(* lazy brute force
   just try removing every element. but the problem is small enough that it doesn't
   matter so might as well *)
let is_increasing2 l =
  let rec aux left right =
    match right with
    | hd :: tl ->
        if is_increasing (List.rev_append left tl) then true
        else aux (hd :: left) tl
    | _ -> false
  in
  aux [] l

let is_decreasing2 l = is_increasing2 (List.rev l)

let solve s inc_fn dec_fn =
  parse s
  |> List.count ~f:(fun line -> inc_fn line || dec_fn line)
  |> Int.to_string |> Ok

let part1 s = solve s is_increasing is_decreasing
let part2 s = solve s is_increasing2 is_decreasing2
