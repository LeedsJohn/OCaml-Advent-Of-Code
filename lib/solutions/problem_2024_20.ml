open! Core
open! Helpers

let of_string s =
  let board = Board.of_string s in
  let start, end_ =
    Map.fold
      board
      ~init:((0, 0), (0, 0))
      ~f:(fun ~key:pos ~data:c (start, end_) ->
        match c with
        | 'S' -> pos, end_
        | 'E' -> start, pos
        | _ -> start, end_)
  in
  let board =
    Map.filter board ~f:(function
      | '.' | 'S' | 'E' -> true
      | _ -> false)
    |> Map.key_set
  in
  board, start, end_
;;

let get_distances_from_pos board pos =
  let res = Hashtbl.create (module Coordinate) in
  Hashtbl.add_exn res ~key:pos ~data:0;
  let rec aux cur_level steps =
    if List.length cur_level = 0
    then ()
    else (
      let neighbors =
        List.map cur_level ~f:Coordinate.neighbors
        |> List.join
        |> Set.of_list (module Coordinate)
        |> Set.to_list
        |> List.filter ~f:(Set.mem board)
        |> List.filter ~f:(fun pos -> not (Hashtbl.mem res pos))
      in
      List.iter neighbors ~f:(fun pos -> Hashtbl.add_exn res ~key:pos ~data:(steps + 1));
      aux neighbors (steps + 1))
  in
  aux [ pos ] 0;
  Map.of_hashtbl_exn (module Coordinate) res
;;

let neighbors_in_radius pos radius =
  List.range (-radius) (radius + 1)
  |> List.map ~f:(fun dx ->
    let remaining_time = radius - Int.abs dx in
    List.range (-remaining_time) (remaining_time + 1) |> List.map ~f:(fun dy -> dx, dy))
  |> List.join
  |> List.map ~f:(fun offset -> Coordinate.add pos offset)
;;

let num_cheats board start end_ time_save cheat_time =
  let distance_from_start = get_distances_from_pos board start in
  let distance_from_end = get_distances_from_pos board end_ in
  let no_cheat_time = Map.find_exn distance_from_start end_ in
  Map.sumi
    (module Int)
    distance_from_start
    ~f:(fun ~key:pos ~data:t1 ->
      neighbors_in_radius pos cheat_time
      |> List.count ~f:(fun end_pos ->
        match Map.find distance_from_end end_pos with
        | None -> false
        | Some t2 ->
          let cheat_time = Coordinate.distance pos end_pos in
          t1 + t2 + cheat_time + time_save <= no_cheat_time))
;;

let part1 s =
  let board, start, end_ = of_string s in
  num_cheats board start end_ 100 2 |> Int.to_string
;;

let part2 s =
  let board, start, end_ = of_string s in
  num_cheats board start end_ 100 20 |> Int.to_string
;;

let%expect_test "neighbors in radius" =
  print_s [%sexp (neighbors_in_radius (0, 0) 1 : Coordinate.t list)];
  [%expect {| ((-1 0) (0 -1) (0 0) (0 1) (1 0)) |}];
  print_s [%sexp (neighbors_in_radius (0, 0) 2 : Coordinate.t list)];
  [%expect
    {|
    ((-2 0) (-1 -1) (-1 0) (-1 1) (0 -2) (0 -1) (0 0) (0 1) (0 2) (1 -1)
     (1 0) (1 1) (2 0))
    |}];
  ()
;;
