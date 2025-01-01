open! Core
open! Helpers

let mult_val = 252533
let mod_val = 33554393
let get_next_pos (x, y) = if y = 0 then (0, x + 1) else (x + 1, y - 1)
let step_val cur_val = cur_val * mult_val % mod_val

let part1 s =
  let goal_row, s = Parse.take_next_int s in
  let goal_col, _ = Parse.take_next_int s in
  let rec aux ((col, row) as pos) cur_val =
    if col + 1 = goal_col && row + 1 = goal_row then cur_val
    else aux (get_next_pos pos) (step_val cur_val)
  in
  aux (0, 0) 20151125 |> Int.to_string |> Ok

let part2 _ = Error (Error.of_string "Unimplemented")
