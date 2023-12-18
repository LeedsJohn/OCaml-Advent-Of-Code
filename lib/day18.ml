open Core

let day = 18
let year = 2023

let parse_text text =
  String.strip text |> String.split_lines
  |> List.map ~f:(fun line ->
         let line = String.split line ~on:' ' in
         let dir = List.hd_exn line |> Char.of_string in
         let num = List.nth_exn line 1 |> Int.of_string in
         let rgb = List.nth_exn line 2 in
         (dir, num, String.slice rgb 2 8))

module Coordinate = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

let dir_to_offsets = function
  | 'R' -> (1, 0)
  | 'L' -> (-1, 0)
  | 'U' -> (0, 1)
  | _ -> (0, -1)

type tile = Trench | Outside | Inside [@@deriving compare, sexp]

let process_instruction board (dir, num, _) start_x start_y =
  let dx, dy = dir_to_offsets dir in
  let new_coordinates =
    List.map
      (List.range 1 (num + 1))
      ~f:(fun i -> (start_x + (i * dx), start_y + (i * dy)))
  in
  let last_x, last_y = List.nth_exn new_coordinates (num - 1) in
  let new_board =
    List.fold new_coordinates ~init:board ~f:(fun acc k ->
        if Map.mem board k then acc else Map.add_exn acc ~key:k ~data:Trench)
  in
  (new_board, last_x, last_y)

let boundaries board =
  List.fold (Map.keys board)
    ~init:(Int.max_value, Int.max_value, Int.min_value, Int.min_value)
    ~f:(fun (x1, y1, x2, y2) (x, y) ->
      (Int.min x1 x, Int.min y1 y, Int.max x2 x, Int.max y2 y))

let flood_fill_outside board =
  let min_x, min_y, max_x, max_y = boundaries board in
  let min_x, min_y, max_x, max_y =
    (min_x - 1, min_y - 1, max_x + 1, max_y + 1)
  in
  let rec aux q board =
    if Fqueue.is_empty q then board
    else
      let (cur_x, cur_y), q = Fqueue.dequeue_exn q in
      if Map.mem board (cur_x, cur_y) then aux q board
      else
        let next_positions =
          [
            (cur_x + 1, cur_y);
            (cur_x - 1, cur_y);
            (cur_x, cur_y + 1);
            (cur_x, cur_y - 1);
          ]
          |> List.filter ~f:(fun (x, y) ->
                 x >= min_x && y >= min_y && x <= max_x && y <= max_y
                 && not (Map.mem board (x, y)))
        in
        let q =
          List.fold next_positions ~init:q ~f:(fun acc pos ->
              Fqueue.enqueue acc pos)
        in
        aux q (Map.add_exn board ~key:(cur_x, cur_y) ~data:Outside)
  in
  aux (Fqueue.singleton (min_x, min_y)) board

(* the outside must already be filled *)
let flood_fill_inside board =
  let min_x, min_y, max_x, max_y = boundaries board in
  List.fold (List.range min_x max_x) ~init:board ~f:(fun acc x ->
      List.fold (List.range min_y max_y) ~init:acc ~f:(fun acc y ->
          if not (Map.mem acc (x, y)) then
            Map.add_exn acc ~key:(x, y) ~data:Inside
          else acc))

let part1 fname =
  let instructions = parse_text (In_channel.read_all fname) in
  let board = Map.singleton (module Coordinate) (0, 0) Trench in
  let board, _, _ =
    List.fold instructions ~init:(board, 0, 0)
      ~f:(fun (board, x, y) instruction ->
        process_instruction board instruction x y)
  in
  let board = flood_fill_outside board in
  let board = flood_fill_inside board in
  List.count (Map.data board) ~f:(fun tile ->
      compare_tile tile Inside = 0 || compare_tile tile Trench = 0)
  |> Int.to_string

(* Part 2 is very different and I don't think I'll be able to reuse much *)
(* I used the Shoelace Theorem to find the area of the polygon and Pick's
   Theorem to find the number of points *)

let parse_text2 text =
  String.strip text |> String.split_lines
  |> List.map ~f:(fun line ->
         let hex = List.nth_exn (String.split line ~on:' ') 2 in
         let hex = String.slice hex 2 8 in
         let num = Int.of_string ("0x" ^ String.slice hex 0 5) in
         let dir =
           match String.get hex 5 with
           | '0' -> 'R'
           | '1' -> 'D'
           | '2' -> 'L'
           | _ -> 'U'
         in
         (num, dir))

let get_vertices instructions =
  let res, _, _ =
    List.fold instructions
      ~init:([ (0, 0) ], 0, 0)
      ~f:(fun (acc, x, y) (num, dir) ->
        let dx, dy = dir_to_offsets dir in
        let end_x, end_y = (x + (dx * num), y + (dy * num)) in
        ((end_x, end_y) :: acc, end_x, end_y))
  in
  List.rev res

(* shoelace theorem *)
let get_inside_area vertices =
  let vertices = Array.of_list vertices in
  let l = Array.length vertices in
  let sum1, sum2 =
    Array.foldi vertices ~init:(0, 0) ~f:(fun i (sum1, sum2) _ ->
        let sum1 = sum1 + (fst vertices.(i) * snd vertices.((i + 1) % l)) in
        let sum2 = sum2 + (snd vertices.(i) * fst vertices.((i + 1) % l)) in
        (sum1, sum2))
  in
  Int.abs (sum1 - sum2) / 2

let part2 fname =
  let instructions = parse_text2 (In_channel.read_all fname) in
  let vertices = get_vertices instructions in
  let perimeter_length =
    List.fold instructions ~init:0 ~f:(fun acc (num, _) -> acc + num)
  in
  (perimeter_length / 2) + get_inside_area vertices + 1 |> Int.to_string

let print_board board =
  let min_x, min_y, max_x, max_y = boundaries board in
  List.iter (List.range max_y min_y ~stride:(-1)) ~f:(fun y ->
      List.iter (List.range min_x max_x) ~f:(fun x ->
          let c =
            match Map.find board (x, y) with
            | None -> '.'
            | Some tile -> (
                match tile with Trench -> '#' | Inside -> 'o' | Outside -> 'O')
          in
          printf "%c" c);
      print_endline "")

let%expect_test "Filling board" =
  let instructions =
    parse_text
      {|R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)|}
  in
  let board = Map.singleton (module Coordinate) (0, 0) Trench in
  let board, _, _ =
    List.fold instructions ~init:(board, 0, 0)
      ~f:(fun (board, x, y) instruction ->
        process_instruction board instruction x y)
  in
  let board = flood_fill_outside board in
  let board = flood_fill_inside board in
  print_board board;
  [%expect
    {|
    OOOOOOOO
    O#######
    O#ooooo#
    O###ooo#
    OOO#ooo#
    OOO#ooo#
    O###o###
    O#ooo#OO
    O##oo###
    OO#oooo#
    OO###### |}];
  ()

let%expect_test "parsing text 2" =
  let instructions =
    parse_text2
      {|R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)|}
  in
  print_s [%sexp (instructions : (int * char) list)];
  [%expect
    {|
    ((461937 R) (56407 D) (356671 R) (863240 D) (367720 R) (266681 D) (577262 L)
     (829975 U) (112010 L) (829975 D) (491645 L) (686074 U) (5411 L) (500254 U)) |}];
  ()
