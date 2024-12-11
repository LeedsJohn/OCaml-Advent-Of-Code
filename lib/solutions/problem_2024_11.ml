open! Core
open! Helpers

let split_num n =
  let s = Int.to_string n in
  let l = String.length s in
  [
    Int.of_string (String.slice s 0 (l / 2));
    Int.of_string (String.slice s (l / 2) 0);
  ]

let get_next_stones n =
  if n = 0 then [ 1 ]
  else if String.length (Int.to_string n) % 2 = 0 then split_num n
  else [ n * 2024 ]

let blink nums = List.map nums ~f:get_next_stones |> List.join

let stones_after_blinks nums blinks =
  let cache = Hashtbl.create (module Coordinate) in
  let rec count_for_stone n blinks =
    if blinks = 0 then 1
    else
      match Hashtbl.find cache (n, blinks) with
      | Some res -> res
      | None ->
          let res =
            get_next_stones n
            |> List.sum
                 (module Int)
                 ~f:(fun n -> count_for_stone n (blinks - 1))
          in
          Hashtbl.add_exn cache ~key:(n, blinks) ~data:res;
          res
  in
  List.sum (module Int) nums ~f:(fun n -> count_for_stone n blinks)

let part1 s =
  let nums = Parse.line_numbers s in
  List.fold (List.range 0 25) ~init:nums ~f:(fun acc _ -> blink acc)
  |> List.length |> Int.to_string |> Ok

let part2 s =
  stones_after_blinks (Parse.line_numbers s) 75 |> Int.to_string |> Ok
