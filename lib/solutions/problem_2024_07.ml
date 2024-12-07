open! Core
open! Helpers

let lines s =
  String.split_lines s
  |> List.map ~f:(fun line ->
         let nums = Parse.line_numbers line in
         (List.hd_exn nums, List.tl_exn nums))

let solve goal nums fns =
  let rec aux cur = function
    | [] -> cur = goal
    | hd :: tl -> List.exists fns ~f:(fun f -> aux (f cur hd) tl)
  in
  aux (List.hd_exn nums) (List.tl_exn nums)

let part1 s =
  lines s
  |> List.sum
       (module Int)
       ~f:(fun (goal, nums) ->
         if solve goal nums [ Int.( + ); Int.( * ) ] then goal else 0)
  |> Int.to_string |> Ok

let part2 s =
  let concat n1 n2 = Int.to_string n1 ^ Int.to_string n2 |> Int.of_string in
  lines s
  |> List.sum
       (module Int)
       ~f:(fun (goal, nums) ->
         if solve goal nums [ Int.( + ); Int.( * ); concat ] then goal else 0)
  |> Int.to_string |> Ok
