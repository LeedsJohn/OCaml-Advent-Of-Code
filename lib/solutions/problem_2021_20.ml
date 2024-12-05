open! Core
open! Helpers

module Game = struct
  type t = { alg : string; board : Set.M(Coordinate).t }

  let bin_to_dec s =
    print_endline s;
    String.foldi (String.rev s) ~init:0 ~f:(fun i acc c ->
        if Char.(c = '#') then acc + Int.pow 2 i else acc)

  let get { board; _ } pos = if Set.mem board pos then '#' else '.'

  let get_alg_index t (x, y) =
    List.map
      (List.range (y - 1) (y + 2))
      ~f:(fun y ->
        List.map
          (List.range (x - 1) (x + 2))
          ~f:(fun x ->
            print_s [%sexp ((x, y, get t (x, y)) : int * int * char)];
            get t (x, y)))
    |> List.join |> String.of_list |> bin_to_dec

  let min_coordinates { board; _ } =
    Set.fold board ~init:(Int.max_value, Int.max_value)
      ~f:(fun (x, y) (x', y') -> (Int.min x x', Int.min y y'))

  let max_coordinates { board; _ } =
    Set.fold board ~init:(Int.min_value, Int.min_value)
      ~f:(fun (x, y) (x', y') -> (Int.max x x', Int.max y y'))

  let print_board t =
    let min_x, min_y = min_coordinates t in
    let max_x, max_y = max_coordinates t in
    print_s [%sexp ((min_x, min_y) : int * int)];
    print_s [%sexp ((max_x, max_y) : int * int)];
    List.iter
      (List.range min_y (max_y + 1))
      ~f:(fun y ->
        List.map (List.range min_x (max_x + 1)) ~f:(fun x -> get t (x, y))
        |> String.of_list |> print_endline)

  let step ({ alg; _ } as t) =
    let min_x, min_y = min_coordinates t in
    let max_x, max_y = max_coordinates t in
    let new_board =
      List.cartesian_product
        (List.range (min_x - 3) (max_x + 4))
        (List.range (min_y - 3) (max_y + 4))
      |> List.fold
           ~init:(Set.empty (module Coordinate))
           ~f:(fun acc pos ->
             let alg_index = get_alg_index t pos in
             match String.get alg alg_index with
             | '#' -> Set.add acc pos
             | _ -> acc)
    in
    { t with board = new_board }

  let of_string s =
    let alg = String.split_lines s |> List.hd_exn in
    let board =
      String.drop_prefix s (String.length alg + 1)
      |> Board.of_string
      |> Map.filter ~f:(Char.equal '#')
      |> Map.key_set
    in
    { alg; board }
end

let part1 s =
  let game = Game.of_string s |> Game.step in
  (* let game = Game.of_string s in *)
  Game.print_board game;
  (* print_s [%sexp (Game.get_alg_index game (2, 3) : int)]; *)
  Set.length game.board |> Int.to_string |> Ok

let part2 _ = Error (Error.of_string "Unimplemented")
