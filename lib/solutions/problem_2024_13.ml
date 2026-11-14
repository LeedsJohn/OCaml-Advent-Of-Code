open! Core
open! Helpers
(* no complicated cases where both buttons have the same slope so that's nice *)

let of_string s =
  let get_coord_from_line line =
    match Parse.line_numbers line with
    | [ a; b ] -> Coordinate.make ~x:a ~y:b
    | _ -> raise_s [%message "not two numbers on line"]
  in
  let rec aux acc lines =
    if List.length lines < 3
    then acc
    else
      aux
        (( List.nth_exn lines 0 |> get_coord_from_line
         , List.nth_exn lines 1 |> get_coord_from_line
         , List.nth_exn lines 2 |> get_coord_from_line )
         :: acc)
        (List.drop lines 4)
  in
  aux [] (String.split_lines s) |> List.rev
;;

let of_string2 s =
  of_string s
  |> List.map ~f:(fun (a, b, coord) ->
    let goal_x, goal_y = Coordinate.xy coord in
    a, b, Coordinate.make ~x:(goal_x + 10000000000000) ~y:(goal_y + 10000000000000))
;;

(* ax + by = e
   cx + dy = f

   acx + bcy = ce
   acx + ady = af

   bcy - ady = ce - af
   y = (ce - af)/(bc - ad) *)
let solve i j goal max_moves =
  let a, c = Coordinate.xy i in
  let b, d = Coordinate.xy j in
  let e, f = Coordinate.xy goal in
  let y = ((c * e) - (a * f)) / ((b * c) - (a * d)) in
  let x =
    let t = Coordinate.sub goal (Coordinate.scale j y) |> Coordinate.x in
    t / a
  in
  let added = Coordinate.add (Coordinate.scale i x) (Coordinate.scale j y) in
  if Coordinate.equal added goal && y < max_moves && x < max_moves
  then (x * 3) + y
  else Int.max_value
;;

let part1 s =
  List.sum
    (module Int)
    (of_string s)
    ~f:(fun (a, b, goal) ->
      let res = solve a b goal 100 in
      if res <> Int.max_value then res else 0)
  |> Int.to_string
;;

let part2 s =
  List.sum
    (module Int)
    (of_string2 s)
    ~f:(fun (a, b, goal) ->
      let res = solve a b goal Int.max_value in
      if res <> Int.max_value then res else 0)
  |> Int.to_string
;;

let%expect_test "stuff" =
  print_s
    [%sexp
      (solve
         (Coordinate.make ~x:1 ~y:1)
         (Coordinate.make ~x:2 ~y:1)
         (Coordinate.make ~x:11 ~y:10)
         100
       : int)];
  [%expect {| 28 |}];
  ()
;;
