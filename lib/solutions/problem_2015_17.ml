open! Core

let num_combos nums goal =
  let rec aux cur = function
    | [] -> Bool.to_int (cur = goal)
    | hd :: tl -> if cur > goal then 0 else aux cur tl + aux (cur + hd) tl
  in
  aux 0 nums

let min_count nums goal =
  let rec aux cur count = function
    | [] -> if cur = goal then count else Int.max_value
    | hd :: tl ->
        if cur > goal then Int.max_value
        else Int.min (aux cur count tl) (aux (cur + hd) (count + 1) tl)
  in
  aux 0 0 nums

let num_with_min_count nums goal =
  let goal_count = min_count nums goal in
  let rec aux cur count = function
    | [] -> Bool.to_int (cur = goal && count = goal_count)
    | hd :: tl ->
        if cur > goal || count > goal then 0
        else aux cur count tl + aux (cur + hd) (count + 1) tl
  in
  aux 0 0 nums

let part1 s =
  let nums = String.split_lines s |> List.map ~f:Int.of_string in
  num_combos nums 150 |> Int.to_string |> Ok

let part2 s =
  let nums = String.split_lines s |> List.map ~f:Int.of_string in
  num_with_min_count nums 150 |> Int.to_string |> Ok
