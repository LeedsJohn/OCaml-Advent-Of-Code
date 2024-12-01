open! Core
open! Helpers

let dir_of_string = function
  | 'R' -> Coordinate.Right
  | 'D' -> Down
  | 'L' -> Left
  | 'U' -> Up
  | _ -> raise_s [%message "bad"]

let thingy = function Coordinate.Up | Down -> Coordinate.Up | _ -> Left

let make_coord_map s =
  let s = String.split s ~on:',' in
  let start = (0, 0) in
  List.fold s
    ~init:(start, Map.empty (module Coordinate))
    ~f:(fun (pos, acc) dir ->
      List.fold
        (List.range 0 (Int.of_string (String.slice dir 1 0)))
        ~init:(pos, acc)
        ~f:(fun (pos, acc) _ ->
          let thing = dir_of_string (String.get dir 0) in
          let next_pos =
            Coordinate.add pos (thing |> Coordinate.direction_to_offset)
          in
          (next_pos, Map.set acc ~key:next_pos ~data:(thingy thing))))
  |> snd

let make_coord_map2 s =
  let s = String.split s ~on:',' in
  let start = (0, 0) in
  let _, res, _ =
    List.fold s
      ~init:(start, Map.empty (module Coordinate), 1)
      ~f:(fun (pos, acc, steps) dir ->
        List.fold
          (List.range 0 (Int.of_string (String.slice dir 1 0)))
          ~init:(pos, acc, steps)
          ~f:(fun (pos, acc, steps) _ ->
            let thing = dir_of_string (String.get dir 0) in
            let next_pos =
              Coordinate.add pos (thing |> Coordinate.direction_to_offset)
            in
            ( next_pos,
              Map.update acc next_pos ~f:(fun v ->
                  match v with Some e -> e | None -> (thingy thing, steps)),
              steps + 1 )))
  in
  res

let dir_equal d1 d2 =
  match (d1, d2) with
  | Coordinate.Up, Coordinate.Left | Left, Up -> false
  | _, _ -> true

let part1 s =
  let l1, l2 =
    match String.split_lines s with
    | [ line1; line2 ] -> (line1, line2)
    | _ -> raise_s [%message "bad lines"]
  in
  let cm1, cm2 = (make_coord_map l1, make_coord_map l2) in
  Map.fold cm1 ~init:Int.max_value ~f:(fun ~key:(x, y) ~data:dir acc ->
      match Map.find cm2 (x, y) with
      | Some dir2 ->
          if not (dir_equal dir dir2) then Int.min acc (Int.abs x + Int.abs y)
          else acc
      | _ -> acc)
  |> Int.to_string |> Ok

let part2 s = 
  let l1, l2 =
    match String.split_lines s with
    | [ line1; line2 ] -> (line1, line2)
    | _ -> raise_s [%message "bad lines"]
  in
  let cm1, cm2 = (make_coord_map2 l1, make_coord_map2 l2) in
  Map.fold cm1 ~init:Int.max_value ~f:(fun ~key:(x, y) ~data:(dir, steps) acc ->
      match Map.find cm2 (x, y) with
      | Some (dir2, steps2) ->
          if not (dir_equal dir dir2) then Int.min acc (steps + steps2)
          else acc
      | _ -> acc)
  |> Int.to_string |> Ok
