open! Core
open! Helpers

type instruction = Toggle | Off | On

let instruction_to_f = function
  | Toggle -> fun b -> not b
  | On -> fun _ -> true
  | Off -> fun _ -> false

let of_string s =
  let get_instruction_type line =
    if String.is_substring line ~substring:"toggle" then Toggle
    else if String.is_substring line ~substring:"off" then Off
    else On
  in
  String.split_lines s
  |> List.map ~f:(fun line ->
         let instruction_type = get_instruction_type line in
         let x1, line = Parse.take_next_int line in
         let y1, line = Parse.take_next_int line in
         let x2, line = Parse.take_next_int line in
         let y2, _ = Parse.take_next_int line in
         ( instruction_type,
           (Int.min x1 x2, Int.min y1 y2),
           (Int.max x1 x2, Int.max y1 y2) ))

let apply_instruction board (instruction_type, (x1, y1), (x2, y2)) =
  let f = instruction_to_f instruction_type in
  List.iter
    (List.range x1 (x2 + 1))
    ~f:(fun x ->
      List.iter
        (List.range y1 (y2 + 1))
        ~f:(fun y -> board.(y).(x) <- f board.(y).(x)))

let count_lights board =
  List.sum
    (module Int)
    (List.range 0 1000)
    ~f:(fun x -> List.count (List.range 0 1000) ~f:(fun y -> board.(y).(x)))

let instruction_to_f2 = function
  | Toggle -> fun n -> n + 2
  | On -> fun n -> n + 1
  | Off -> fun n -> Int.max 0 (n - 1)

let apply_instruction2 board (instruction_type, (x1, y1), (x2, y2)) =
  let f = instruction_to_f2 instruction_type in
  List.iter
    (List.range x1 (x2 + 1))
    ~f:(fun x ->
      List.iter
        (List.range y1 (y2 + 1))
        ~f:(fun y -> board.(y).(x) <- f board.(y).(x)))

let count_lights2 board =
  List.sum
    (module Int)
    (List.range 0 1000)
    ~f:(fun x ->
      List.sum (module Int) (List.range 0 1000) ~f:(fun y -> board.(y).(x)))

let part1 s =
  let board = Array.make_matrix ~dimx:1000 ~dimy:1000 false in
  of_string s |> List.iter ~f:(apply_instruction board);
  count_lights board |> Int.to_string |> Ok

let part2 s =
  let board = Array.make_matrix ~dimx:1000 ~dimy:1000 0 in
  of_string s |> List.iter ~f:(apply_instruction2 board);
  count_lights2 board |> Int.to_string |> Ok
