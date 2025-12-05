open! Core
open Helpers

let get_rolls_to_remove board =
  Map.filteri board ~f:(fun ~key:coord ~data:c ->
    if Char.( <> ) c '@'
    then false
    else
      Coordinate.neighbors8 coord
      |> List.count ~f:(fun coord ->
        Char.( = ) (Map.find board coord |> Option.value ~default:' ') '@')
      < 4)
  |> Map.keys
;;

let part1 s =
  let board = Board.of_string s in
  Map.counti board ~f:(fun ~key:coord ~data:c ->
    if Char.( <> ) c '@'
    then false
    else
      Coordinate.neighbors8 coord
      |> List.count ~f:(fun coord ->
        Char.( = ) (Map.find board coord |> Option.value ~default:' ') '@')
      < 4)
  |> Int.to_string
;;

let part2 s =
  let board = Board.of_string s in
  let rec solve board res =
    let rolls = get_rolls_to_remove board in
    if List.length rolls = 0
    then res
    else solve (List.fold rolls ~init:board ~f:Map.remove) (res + List.length rolls)
  in
  solve board 0 |> Int.to_string
;;
