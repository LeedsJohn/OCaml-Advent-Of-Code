open! Core
open! Helpers

let part1 s =
  String.split_lines s
  |> List.count ~f:(fun line ->
         match List.sort (Parse.line_numbers line) ~compare:Int.compare with
         | [ a; b; c ] -> a + b > c
         | _ -> false)
  |> Int.to_string |> Ok

let part2 s =
  let triangle_nums a b c =
    match List.sort [ a; b; c ] ~compare:Int.compare with
    | [ a; b; c ] -> a + b > c
    | _ -> false
  in
  let rec aux acc = function
    | [] -> acc
    | a :: b :: c :: tl ->
        let a = Parse.line_numbers a |> List.to_array in
        let b = Parse.line_numbers b |> List.to_array in
        let c = Parse.line_numbers c |> List.to_array in
        let acc =
          acc
          + List.count (List.range 0 3) ~f:(fun i ->
                triangle_nums a.(i) b.(i) c.(i))
        in
        aux acc tl
    | _ -> Int.max_value
  in
  String.split_lines s |> aux 0 |> Int.to_string |> Ok
