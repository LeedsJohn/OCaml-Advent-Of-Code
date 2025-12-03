open! Core
open! Helpers

let dim = 70

let of_string s num =
  let s = String.split_lines s in
  List.take s num
  |> List.map ~f:(fun line ->
    let x, line = Parse.take_next_int line in
    let y, _ = Parse.take_next_int line in
    x, y)
  |> Set.of_list (module Coordinate)
;;

let bfs corrupted =
  let rec aux corrupted level steps =
    if Set.length level = 0
    then -1
    else if Set.mem level (dim, dim)
    then steps
    else (
      let neighbors =
        Set.to_list level
        |> List.map ~f:Coordinate.neighbors
        |> List.join
        |> Set.of_list (module Coordinate)
        |> Set.filter ~f:(fun (x, y) ->
          Int.min x y >= 0 && Int.max x y <= dim && not (Set.mem corrupted (x, y)))
      in
      aux (Set.union corrupted neighbors) neighbors (steps + 1))
  in
  aux (Set.add corrupted (0, 0)) (Set.singleton (module Coordinate) (0, 0)) 0
;;

let has_path s num_bytes =
  let corrupted = of_string s num_bytes in
  bfs corrupted <> -1
;;

let part1 s =
  let corrupted = of_string s 1024 in
  bfs corrupted |> Int.to_string
;;

let part2 s =
  let lines = String.split_lines s in
  let rec bin_search lo hi =
    let m = (lo + hi) / 2 in
    if has_path s m
    then if not (has_path s (m + 1)) then m else bin_search (m + 1) hi
    else bin_search lo (m - 1)
  in
  let n = bin_search 0 (List.length lines) in
  List.nth_exn lines n
;;
