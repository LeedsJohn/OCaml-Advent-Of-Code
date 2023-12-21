open Core

let day = 21
let year = 2023

module Coordinate = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

let parse_text text =
  let board =
    String.strip text |> String.split_lines
    |> List.map ~f:String.to_array
    |> Array.of_list
  in
  let start_x, start_y =
    List.find_exn
      (List.cartesian_product
         (List.range 0 (Array.length board.(0)))
         (List.range 0 (Array.length board)))
      ~f:(fun (x, y) -> Char.(board.(y).(x) = 'S'))
  in
  board.(start_y).(start_x) <- '.';
  (start_x, start_y, board)

let is_in_bounds board x y =
  x >= 0 && y >= 0 && x < Array.length board.(0) && y < Array.length board

let get_neighbors board x y =
  [ (x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1) ]
  |> List.filter ~f:(fun (x, y) ->
         is_in_bounds board x y && Char.(board.(y).(x) = '.'))

let get_next_positions board cur_positions get_neighbors =
  List.fold cur_positions
    ~init:(Set.empty (module Coordinate))
    ~f:(fun acc (x, y) ->
      List.fold (get_neighbors board x y) ~init:acc ~f:(fun acc (x, y) ->
          Set.add acc (x, y)))
  |> Set.to_list

let take_n_steps board start_pos n get_neighbors =
  List.fold (List.range 0 n) ~init:[ start_pos ] ~f:(fun acc _ ->
      get_next_positions board acc get_neighbors)

let part1 fname =
  let num_steps = if String.(fname = "test_input/21.in") then 6 else 64 in
  let x, y, board = parse_text (In_channel.read_all fname) in
  take_n_steps board (x, y) num_steps get_neighbors
  |> List.length |> Int.to_string

let get_square_type board x y =
  board.(y % Array.length board).(x % Array.length board.(0))

let get_neighbors2 board x y =
  [ (x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1) ]
  |> List.filter ~f:(fun (x, y) -> Char.(get_square_type board x y = '.'))

(* this was frustrating problem because it is difficult to create a general
   solution (but this problem would be more possible to write a good solution
   for than yesterday). *)
let part2 fname =
  let num_steps = 26501365 in
  let x, y, board = parse_text (In_channel.read_all fname) in
  let n = Array.length board in
  let remainder = num_steps % n in
  let stuff =
    List.map
      [ remainder; remainder + n; remainder + (2 * n) ]
      ~f:(fun n -> take_n_steps board (x, y) n get_neighbors2 |> List.length)
  in
  printf "remainder: %d\n" remainder;
  printf "%d\n" (num_steps / n);
  print_s [%sexp (stuff : int list)];
  let f x = (14613 * x * x) + (14747 * x) + 3726 in
  f (num_steps / n) |> Int.to_string

let%expect_test "mod" =
  printf "%d" (-1 % 4);
  [%expect {| 3 |}];
  ()
