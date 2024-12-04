open! Core
open! Helpers

let part1 s =
  let board = Board.of_string s in
  let get_word start dir =
    List.map (Coordinate.shoot_ray ~start ~dir ~length:4) ~f:(fun pos ->
        Map.find board pos |> Option.value ~default:'.')
    |> String.of_list
  in
  let all_words_from_spot start =
    List.map Coordinate.offsets8 ~f:(fun dir -> get_word start dir)
  in
  List.map (Map.keys board) ~f:all_words_from_spot
  |> List.join
  |> List.count ~f:(String.equal "XMAS")
  |> Int.to_string |> Ok

let part2 s =
  let board = Board.of_string s in
  let dir_is_mas start dir =
    Coordinate.shoot_ray
      ~start:(Coordinate.multiply dir (-1) |> Coordinate.add start)
      ~dir ~length:3
    |> List.map ~f:(fun pos -> Map.find board pos |> Option.value ~default:'.')
    |> String.of_list |> String.equal "MAS"
  in
  let is_xmas pos =
    List.exists
      (List.cartesian_product Coordinate.diagonal_offsets
         Coordinate.diagonal_offsets) ~f:(fun (dir1, dir2) ->
        (not (Coordinate.equal dir1 dir2))
        && dir_is_mas pos dir1 && dir_is_mas pos dir2)
  in
  Map.keys board
  |> List.sum (module Int) ~f:(fun pos -> is_xmas pos |> Bool.to_int)
  |> Int.to_string |> Ok
