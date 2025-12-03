open! Core
open! Helpers

let is_wall (x, y) num =
  if Int.min x y < 0
  then true
  else (
    let n = (x * x) + (3 * x) + (2 * x * y) + y + (y * y) + num in
    Int.popcount n % 2 = 1)
;;

let is_open pos num = not (is_wall pos num)

let bfs start_pos end_pos num =
  let rec aux cur visited steps =
    if Set.length cur = 0
    then Int.max_value
    else if Set.mem cur end_pos
    then steps
    else (
      let visited = Set.union visited cur in
      let neighbors =
        Set.to_list cur
        |> List.map ~f:Coordinate.neighbors
        |> List.join
        |> List.filter ~f:(fun pos -> is_open pos num)
        |> List.filter ~f:(fun pos -> not (Set.mem visited pos))
        |> Set.of_list (module Coordinate)
      in
      aux neighbors visited (steps + 1))
  in
  aux (Set.singleton (module Coordinate) start_pos) (Set.empty (module Coordinate)) 0
;;

let bfs2 start_pos num_steps num =
  let rec aux cur visited steps =
    if steps > num_steps
    then Set.length visited
    else (
      let visited = Set.union visited cur in
      let neighbors =
        Set.to_list cur
        |> List.map ~f:Coordinate.neighbors
        |> List.join
        |> List.filter ~f:(fun pos -> is_open pos num)
        |> List.filter ~f:(fun pos -> not (Set.mem visited pos))
        |> Set.of_list (module Coordinate)
      in
      aux neighbors visited (steps + 1))
  in
  aux (Set.singleton (module Coordinate) start_pos) (Set.empty (module Coordinate)) 0
;;

let _print_board max_x max_y num =
  List.map (List.range 0 max_y) ~f:(fun y ->
    List.map (List.range 0 max_x) ~f:(fun x -> if is_wall (x, y) num then '#' else '.')
    |> String.of_list)
  |> String.concat ~sep:"\n"
  |> print_endline
;;

let part1 s =
  let num = Int.of_string s in
  bfs (1, 1) (31, 39) num |> Int.to_string
;;

let part2 s =
  let num = Int.of_string s in
  bfs2 (1, 1) 50 num |> Int.to_string
;;
