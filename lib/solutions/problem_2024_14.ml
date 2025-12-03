open! Core
open! Helpers

let max_x = 101
let max_y = 103

let of_string s =
  let parse_line line =
    match Parse.line_numbers line with
    | [ x; y; dx; dy ] -> (x, y), (dx, dy)
    | _ -> raise_s [%message "malformed line"]
  in
  String.split_lines s |> List.map ~f:parse_line
;;

let step_robot ((x, y), (dx, dy)) =
  let step a da m =
    let a = a + da in
    let a = if a < 0 then a + m else a in
    a % m
  in
  let x = step x dx max_x in
  let y = step y dy max_y in
  (x, y), (dx, dy)
;;

let step_robots robots = List.map robots ~f:step_robot

let contains_tree robo =
  let robots = List.map robo ~f:fst |> Set.of_list (module Coordinate) in
  (* solution 1 : look for a horizontal line *)
  Set.exists robots ~f:(fun (x, y) ->
    List.for_all (List.range 1 10) ~f:(fun dx -> Set.mem robots (x + dx, y)))
;;

(* solution 2 : look for a layout where all robots are on unique points *)
(* Set.length robots = List.length robo *)

let count_quadrant robots min_x max_x min_y max_y =
  List.count robots ~f:(fun ((x, y), _) ->
    Int.between x ~low:min_x ~high:max_x && Int.between y ~low:min_y ~high:max_y)
;;

(* let print_robots robots =
   let board =
     Array.init max_y ~f:(fun _ -> Array.init max_x ~f:(fun _ -> '.'))
   in
   List.iter robots ~f:(fun ((x, y), _) -> board.(y).(x) <- '#');
   Array.map board ~f:String.of_array |> Array.iter ~f:print_endline;
   print_endline "------------" *)

let part1 s =
  let robots = of_string s in
  let robots =
    List.fold (List.range 0 100) ~init:robots ~f:(fun acc _ -> step_robots acc)
  in
  let mid_x = max_x / 2 in
  let mid_y = max_y / 2 in
  let quads =
    [ 0, mid_x - 1, 0, mid_y - 1
    ; mid_x + 1, max_x - 1, 0, mid_y - 1
    ; 0, mid_x - 1, mid_y + 1, max_y - 1
    ; mid_x + 1, max_x - 1, mid_y + 1, max_y - 1
    ]
  in
  print_s [%sexp (quads : (int * int * int * int) list)];
  List.fold quads ~init:1 ~f:(fun acc (x1, x2, y1, y2) ->
    let n = count_quadrant robots x1 x2 y1 y2 in
    print_s [%sexp (n : int)];
    acc * count_quadrant robots x1 x2 y1 y2)
  |> Int.to_string
;;

let part2 s =
  let rec aux robots steps =
    if contains_tree robots then steps else aux (step_robots robots) (steps + 1)
  in
  aux (of_string s) 0 |> Int.to_string
;;
