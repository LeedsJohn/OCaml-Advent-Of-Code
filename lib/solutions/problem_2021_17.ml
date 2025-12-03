open! Core
open! Helpers

(* i could do this with math but that's LAME! *)

module Box = struct
  type t =
    { min_x : int
    ; max_x : int
    ; min_y : int
    ; max_y : int
    }
  [@@deriving sexp]

  let of_string s =
    let s = String.drop_prefix s 15 in
    let min_x, s = Parse.take_int s in
    let max_x, s = Parse.take_int (String.drop_prefix s 2) in
    let min_y, s = Parse.take_int (String.drop_prefix s 4) in
    let max_y, _ = Parse.take_int (String.drop_prefix s 2) in
    { min_x; max_x; min_y; max_y }
  ;;
end

module Game = struct
  type t =
    { box : Box.t
    ; position : Coordinate.t
    ; velocity : Coordinate.t
    }
  [@@deriving sexp_of]

  let create ~box ~velocity = { box; velocity; position = 0, 0 }

  let tick { position; velocity; box } =
    let dx, _ = velocity in
    let john = if dx = 0 then 0 else if dx < 0 then 1 else -1 in
    { box
    ; position = Coordinate.add position velocity
    ; velocity = Coordinate.add velocity (john, -1)
    }
  ;;

  let in_box { position = x, y; box = { min_x; max_x; min_y; max_y }; _ } =
    Int.between x ~low:min_x ~high:max_x && Int.between y ~low:min_y ~high:max_y
  ;;

  let rec first_game_below_cutoff ({ position = _, y; box = { max_y; _ }; _ } as t) =
    if y <= max_y then t else first_game_below_cutoff (tick t)
  ;;

  let rec intersects game =
    let x, y = game.position in
    if in_box game
    then true
    else if y < game.box.min_y || x > game.box.max_x
    then false
    else intersects (tick game)
  ;;

  let rec highest_y ({ position = _, y; velocity = _, dy; _ } as t) =
    if dy <= 0 then y else highest_y (tick t)
  ;;
end

let test_y_value box y =
  let rec aux x =
    let game = Game.create ~box ~velocity:(x, y) in
    let game = Game.first_game_below_cutoff game in
    if Game.in_box game
    then true
    else (
      let x', _ = game.position in
      let dx, _ = game.velocity in
      if x' - dx + 1 > game.box.max_x then false else aux (x + 1))
  in
  aux 1
;;

let part1 s =
  let box = Box.of_string s in
  let rec aux y =
    if test_y_value box y
    then (
      let game = Game.create ~box ~velocity:(0, y) in
      Game.highest_y game)
    else aux (y - 1)
  in
  aux 10000 |> Int.to_string
;;

let part2 s =
  let box = Box.of_string s in
  List.fold (List.range 0 228) ~init:0 ~f:(fun acc x ->
    List.fold (List.range (-150) 10000) ~init:acc ~f:(fun acc y ->
      acc + (Game.intersects (Game.create ~box ~velocity:(x, y)) |> Bool.to_int)))
  |> Int.to_string
;;

let%expect_test "stuff" =
  let box = Box.of_string "target area: x=20..30, y=-10..-5" in
  print_s
    [%sexp (Game.first_game_below_cutoff (Game.create ~box ~velocity:(6, 0)) : Game.t)];
  [%expect
    {|
      ((box ((min_x 20) (max_x 30) (min_y -10) (max_y -5))) (position (18 -6))
       (velocity (2 -4)))
      |}]
;;
