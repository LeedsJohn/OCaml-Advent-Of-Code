open! Core
open! Helpers

let lines s =
  String.split_lines s
  |> List.map ~f:(fun line ->
         let goal, s = Parse.take_int line in
         let rec get_nums acc s =
           if String.equal s "" then List.rev acc
           else if Char.is_digit (String.get s 0) then
             let num, s = Parse.take_int s in
             get_nums (num :: acc) (String.drop_prefix s 1)
           else get_nums acc (String.drop_prefix s 1)
         in
         (goal, get_nums [] s))

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
