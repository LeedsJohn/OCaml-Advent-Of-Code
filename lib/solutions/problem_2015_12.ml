open! Core
open! Helpers

let part1 s =
  String.split_lines s
  |> List.sum
       (module Int)
       ~f:(fun line ->
         Parse.line_numbers line |> List.sum (module Int) ~f:Fn.id)
  |> Int.to_string |> Ok

let part2 _ = Error (Error.of_string "Unimplemented")
