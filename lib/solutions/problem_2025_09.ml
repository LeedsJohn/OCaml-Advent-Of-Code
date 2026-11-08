open! Core
open Helpers

let parse s =
  let coords =
    String.split_lines s
    |> List.map ~f:Parse.line_numbers
    |> List.map ~f:(function
      | [ x; y ] -> x, y
      | _ -> raise_s [%message "bruh"])
  in
  let min_x, min_y =
    List.fold coords ~init:(Int.max_value, Int.max_value) ~f:(fun (mx, my) (x, y) ->
      Int.min mx x, Int.min my y)
  in
  List.map coords ~f:(fun c -> Coordinate.sub c (min_x, min_y))
;;

let rectangle_area (x1, y1) (x2, y2) = (Int.abs (x1 - x2) + 1) * (Int.abs (y1 - y2) + 1)

let part1 s =
  let coords = parse s in
  List.cartesian_product coords coords
  |> List.map ~f:(fun (c1, c2) -> rectangle_area c1 c2)
  |> List.max_elt ~compare:Int.compare
  |> Option.value_exn
  |> Int.to_string
;;

module Coord_map = struct
  type t =
    { x_map : int array
    ; y_map : int array
    }

  let create coords =
    let make_side get_fn =
      List.map coords ~f:get_fn
      |> List.dedup_and_sort ~compare:Int.compare
      |> List.fold ~init:[] ~f:(fun acc n ->
        match acc with
        | [] -> [ n ]
        | hd :: _ -> if hd + 1 = n then n :: acc else n :: (hd + 1) :: acc)
      |> List.rev
      |> List.to_array
    in
    { x_map = make_side fst; y_map = make_side snd }
  ;;

  let of_real_coord_exn { x_map; y_map } (x, y) =
    let calc_side map n =
      if not (Int.between n ~low:map.(0) ~high:map.(Array.length map - 1))
      then raise_s [%message "something is very wrong"]
      else
        Array.binary_search map ~compare:Int.compare `Last_less_than_or_equal_to n
        |> Option.value_exn
    in
    calc_side x_map x, calc_side y_map y
  ;;
end

type tile =
  | Corner
  | Filled
  | Empty

let make_board corners : tile array array =
  let max_x, max_y =
    List.fold corners ~init:(Int.min_value, Int.min_value) ~f:(fun (mx, my) (x, y) ->
      Int.max mx x, Int.max my y)
  in
  let board = Array.init (max_y + 1) ~f:(fun _ -> Array.create ~len:(max_x + 1) None) in
  let draw_border (x1, y1) (x2, y2) =
    board.(y1).(x1) <- Some Corner;
    board.(y2).(x2) <- Some Corner;
    let dir =
      if x1 < x2 then 1, 0 else if x1 > x2 then -1, 0 else if y1 < y2 then 0, 1 else 0, -1
    in
    let rec coords_between = function
      | [] -> coords_between [ Coordinate.add (x1, y1) dir ]
      | hd :: tl ->
        if Coordinate.equal hd (x2, y2)
        then tl
        else coords_between (Coordinate.add hd dir :: hd :: tl)
    in
    coords_between [] |> List.iter ~f:(fun (x, y) -> board.(y).(x) <- Some Filled)
  in
  let rec aux = function
    | [] -> raise_s [%message "this should not be empty"]
    | [ c ] -> draw_border c (List.hd_exn corners)
    | a :: b :: tl ->
      draw_border a b;
      aux (b :: tl)
  in
  aux corners;
  let rec fill (x, y) visited =
    if not (Int.between x ~low:0 ~high:max_x && Int.between y ~low:0 ~high:max_y)
    then Empty
    else (
      match board.(y).(x) with
      | Some t -> t
      | None ->
        board.(y).(x)
        <- (if
              Coordinate.neighbors (x, y)
              |> List.filter ~f:(Fn.compose not (Set.mem visited))
              |> List.exists ~f:(fun c ->
                match fill c (Set.add visited c) with
                | Empty -> true
                | Corner | Filled -> false)
            then Some Empty
            else Some Filled);
        Option.value_exn board.(y).(x))
  in
  List.cartesian_product (List.range 0 (max_x + 1)) (List.range 0 (max_y + 1))
  |> List.iter ~f:(fun c -> ignore (fill c (Set.singleton (module Coordinate) c)));
  (* idk why Array.map board ~f:(Array.map ~f:Option.value_exn) wasn't working *)
  Array.map board ~f:(fun row -> Array.map row ~f:(fun tile -> Option.value_exn tile))
;;

let is_rectangle board (x1, y1) (x2, y2) =
  List.cartesian_product
    (List.range (Int.min x1 x2) (Int.max x1 x2 + 1))
    (List.range (Int.min y1 y2) (Int.max y1 y2 + 1))
  |> List.for_all ~f:(fun (x, y) ->
    match board.(y).(x) with
    | Corner | Filled -> true
    | Empty -> false)
;;

let part2 s =
  let corners = parse s in
  let coord_map = Coord_map.create corners in
  let adjusted_corners = List.map corners ~f:(Coord_map.of_real_coord_exn coord_map) in
  let board = make_board adjusted_corners in
  List.cartesian_product corners corners
  |> List.filter ~f:(fun ((x1, y1), (x2, y2)) -> x1 < x2 || y1 < y2)
  |> List.fold ~init:0 ~f:(fun acc (c1, c2) ->
    let a = rectangle_area c1 c2 in
    if
      a > acc
      && is_rectangle
           board
           (Coord_map.of_real_coord_exn coord_map c1)
           (Coord_map.of_real_coord_exn coord_map c2)
    then a
    else acc)
  |> Int.to_string
;;
