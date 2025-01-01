open! Core
open! Helpers

let char_to_dir = function
  | 'U' -> (0, -1)
  | 'D' -> (0, 1)
  | 'L' -> (-1, 0)
  | 'R' -> (1, 0)
  | c -> raise_s [%message "bad char character" (c : char)]

let of_string s ~board_string =
  let board = Board.of_string board_string |> Map.filter ~f:(Char.( <> ) ' ') in
  let lines =
    String.split_lines s
    |> List.map ~f:(fun line -> String.to_list line |> List.map ~f:char_to_dir)
  in
  (board, lines)

let apply_moves board pos moves =
  List.fold moves ~init:pos ~f:(fun pos dir ->
      let next_pos = Coordinate.add pos dir in
      if Map.mem board next_pos then next_pos else pos)

let part1 s =
  let board, lines = of_string s ~board_string:{|123
456
789|} in
  List.fold lines
    ~init:("", (1, 1))
    ~f:(fun (res, pos) line ->
      let pos = apply_moves board pos line in
      (res ^ String.of_char (Map.find_exn board pos), pos))
  |> fst |> Ok

let part2 s =
  let board, lines = of_string s ~board_string:{|  1
 234
56789
 ABC
  D|} in
  List.fold lines
    ~init:("", (0, 2))
    ~f:(fun (res, pos) line ->
      let pos = apply_moves board pos line in
      (res ^ String.of_char (Map.find_exn board pos), pos))
  |> fst |> Ok
