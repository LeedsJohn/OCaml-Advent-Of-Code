open! Core
open! Helpers

let of_string s =
  let get_coord_from_line line =
    match Parse.line_numbers line with
    | [ a; b ] -> (a, b)
    | _ -> raise_s [%message "not two numbers on line"]
  in
  let rec aux acc lines =
    if List.length lines < 3 then acc
    else
      aux
        (( List.nth_exn lines 0 |> get_coord_from_line,
           List.nth_exn lines 1 |> get_coord_from_line,
           List.nth_exn lines 2 |> get_coord_from_line )
        :: acc)
        (List.drop lines 4)
  in
  aux [] (String.split_lines s) |> List.rev

let of_string2 s =
  of_string s
  |> List.map ~f:(fun (a, b, (goal_x, goal_y)) ->
         (a, b, (goal_x + 10000000000000, goal_y + 10000000000000)))

let solve a ((xb, _) as b) ((goal_x, goal_y) as goal) move_limit =
  let rec aux (x, y) moves =
    if x > goal_x || y > goal_y || moves > move_limit then Int.max_value
    else
      let steps_to_end = (goal_x - x) / xb in
      let new_pos = Coordinate.add (x, y) (Coordinate.scale b steps_to_end) in
      if
        Coordinate.equal new_pos goal
        && moves <= move_limit && steps_to_end <= move_limit
      then
        Int.min
          ((moves * 3) + steps_to_end)
          (aux (Coordinate.add (x, y) a) (moves + 1))
      else aux (Coordinate.add (x, y) a) (moves + 1)
  in
  aux (0, 0) 0

let part1 s =
  List.sum
    (module Int)
    (of_string s)
    ~f:(fun (a, b, goal) ->
      let res = solve a b goal 100 in
      if res <> Int.max_value then res else 0)
  |> Int.to_string |> Ok

let part2 s =
  List.sum
    (module Int)
    (of_string2 s)
    ~f:(fun (a, b, goal) ->
        print_endline "yeah";
      let res = solve a b goal Int.max_value in
      if res <> Int.max_value then res else 0)
  |> Int.to_string |> Ok

let%expect_test "stuff" =
  print_s [%sexp (solve (1, 1) (2, 1) (11, 10) 100 : int)];
  [%expect {||}];
  ()
