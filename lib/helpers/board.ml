open! Core

type 'a t = 'a Map.M(Coordinate).t [@@deriving equal, compare, sexp_of]

let of_string s =
  List.foldi
    (String.split_lines s)
    ~init:(Map.empty (module Coordinate))
    ~f:(fun y acc line ->
      String.foldi line ~init:acc ~f:(fun x acc c -> Map.add_exn acc ~key:(x, y) ~data:c))
;;

let min_coordinates t =
  Map.fold t ~init:(Int.max_value, Int.max_value) ~f:(fun ~key:(x', y') ~data:_ (x, y) ->
    Int.min x x', Int.min y y')
;;

let max_coordinates t =
  Map.fold t ~init:(Int.min_value, Int.min_value) ~f:(fun ~key:(x', y') ~data:_ (x, y) ->
    Int.max x x', Int.max y y')
;;

let to_string t =
  let min_x, min_y = min_coordinates t in
  let max_x, max_y = max_coordinates t in
  List.fold
    (List.range min_y (max_y + 1))
    ~init:[]
    ~f:(fun acc y ->
      let line =
        List.fold
          (List.range min_x (max_x + 1))
          ~init:[]
          ~f:(fun acc x ->
            let c = Map.find t (x, y) |> Option.value ~default:' ' in
            c :: acc)
      in
      (List.rev line |> String.of_list) :: acc)
  |> List.rev
  |> String.concat_lines
;;
