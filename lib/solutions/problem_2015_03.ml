open! Core
open! Helpers

let get_dir = function
  | '>' -> 1, 0
  | '<' -> -1, 0
  | 'v' -> 0, 1
  | '^' -> 0, -1
  | c -> raise_s [%message "wtf" (c : char)]
;;

let part1 s =
  String.fold
    s
    ~init:((0, 0), Set.singleton (module Coordinate) (0, 0))
    ~f:(fun (pos, s) dir ->
      let dir = get_dir dir in
      let new_pos = Coordinate.add pos dir in
      new_pos, Set.add s new_pos)
  |> snd
  |> Set.length
  |> Int.to_string
;;

let part2 s =
  let _, _, res =
    String.foldi
      s
      ~init:((0, 0), (0, 0), Set.singleton (module Coordinate) (0, 0))
      ~f:(fun i (pos1, pos2, s) dir ->
        let dir = get_dir dir in
        if i % 2 = 0
        then (
          let new_pos = Coordinate.add pos1 dir in
          new_pos, pos2, Set.add s new_pos)
        else (
          let new_pos = Coordinate.add pos2 dir in
          pos1, new_pos, Set.add s new_pos))
  in
  Set.length res |> Int.to_string
;;
