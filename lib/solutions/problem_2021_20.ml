open! Core
open! Helpers

module Game = struct
  type t = { alg : string; board : Set.M(Coordinate).t }

  let bin_to_dec s =
      String.foldi (String.rev s) ~init:0 ~f:(fun i acc c ->
          if Char.(c = '#') then acc + (Int.pow 2 i) else acc
      )

  let get { board; _ } pos = if Set.mem board pos then '#' else '.'

  let get_alg_index t pos =
    List.map (List.range (-1) 2) ~f:(fun dy ->
        List.map (List.range (-1) 2) ~f:(fun dx ->
            let p = Coordinate.add pos (dx, dy) in
            get t p))
    |> List.join |> String.of_list |> bin_to_dec

  let step ({ alg; _ } as t) =
    let new_board =
      List.cartesian_product (List.range (-100) 100) (List.range (-100) 100)
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
  let game = Game.of_string s |> Game.step |> Game.step in
  Set.length game.board |> Int.to_string |> Ok

let part2 _ = Error (Error.of_string "Unimplemented")
