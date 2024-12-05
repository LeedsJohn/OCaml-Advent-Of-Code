open! Core
open! Helpers

module Game = struct
  type t = { alg : string; board : char Board.t; default : char }

  let bin_to_dec s =
    String.foldi (String.rev s) ~init:0 ~f:(fun i acc c ->
        if Char.(c = '#') then acc + Int.pow 2 i else acc)

  let get { board; default; _ } pos =
    Map.find board pos |> Option.value ~default

  let get_alg_index t (x, y) =
    List.map
      (List.range (y - 1) (y + 2))
      ~f:(fun y ->
        List.map (List.range (x - 1) (x + 2)) ~f:(fun x -> get t (x, y)))
    |> List.join |> String.of_list |> bin_to_dec

  let _print_board t =
    let min_x, min_y = Board.min_coordinates t.board in
    let max_x, max_y = Board.max_coordinates t.board in
    List.iter
      (List.range min_y (max_y + 1))
      ~f:(fun y ->
        List.map (List.range min_x (max_x + 1)) ~f:(fun x -> get t (x, y))
        |> String.of_list |> print_endline)

  let step ({ alg; default; board } as t) =
    let min_x, min_y = Board.min_coordinates board in
    let max_x, max_y = Board.max_coordinates board in
    let new_board =
      List.cartesian_product
        (List.range (min_x - 2) (max_x + 2))
        (List.range (min_y - 2) (max_y + 2))
      |> List.fold
           ~init:(Map.empty (module Coordinate))
           ~f:(fun acc pos ->
             let alg_index = get_alg_index t pos in
             Map.add_exn acc ~key:pos ~data:(String.get alg alg_index))
    in
    {
      t with
      board = new_board;
      default = (if Char.(default = '#') then '.' else '#');
    }

  let of_string s =
    let alg = String.split_lines s |> List.hd_exn in
    let board =
      String.drop_prefix s (String.length alg + 1) |> Board.of_string
    in
    { alg; board; default = '.' }
end

let part1 s =
  let game = Game.of_string s |> Game.step |> Game.step in
  Map.count game.board ~f:(Char.equal '#') |> Int.to_string |> Ok

let part2 s =
  let game =
    List.fold (List.range 0 50) ~init:(Game.of_string s) ~f:(fun acc _ ->
        Game.step acc)
  in
  Map.count game.board ~f:(Char.equal '#') |> Int.to_string |> Ok
