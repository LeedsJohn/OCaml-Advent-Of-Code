open! Core
open! Helpers

module Instruction = struct
  type t =
    | Rect of int * int
    | Rotate_row of int * int
    | Rotate_col of int * int
  [@@deriving sexp_of]

  let of_string s =
    let num1, num2 =
      match Parse.line_numbers s with
      | [ a; b ] -> a, b
      | _ -> raise_s [%message "wtf"]
    in
    let ar = String.split s ~on:' ' |> List.to_array in
    match ar.(0) with
    | "rect" -> Rect (num1, num2)
    | _ ->
      (match ar.(1) with
       | "column" -> Rotate_col (num1, num2)
       | _ -> Rotate_row (num1, num2))
  ;;
end

module Display = struct
  type t = Set.M(Coordinate).t

  let length = 50
  let height = 6
  let get t pos = Set.mem t pos

  let set t pos status =
    match get t pos with
    | true -> if not status then Set.remove t pos else t
    | false -> if status then Set.add t pos else t
  ;;

  let rect t length width : t =
    List.cartesian_product (List.range 0 length) (List.range 0 width)
    |> List.fold ~init:t ~f:(fun acc pos -> set acc pos true)
  ;;

  let rotate_row (t : t) move_y n : t =
    Set.map
      (module Coordinate)
      t
      ~f:(fun ((x, y) as pos) -> if y <> move_y then pos else (x + n) % length, y)
  ;;

  let rotate_col (t : t) move_x n : t =
    Set.map
      (module Coordinate)
      t
      ~f:(fun ((x, y) as pos) -> if x <> move_x then pos else x, (y + n) % height)
  ;;

  let apply_instruction t = function
    | (Rect (x, y) : Instruction.t) -> rect t x y
    | Rotate_row (row, n) -> rotate_row t row n
    | Rotate_col (col, n) -> rotate_col t col n
  ;;

  let num_pixels = Set.length

  let print t =
    List.cartesian_product (List.range 0 length) (List.range 0 height)
    |> List.fold
         ~init:(Map.empty (module Coordinate))
         ~f:(fun acc pos ->
           let data = if Set.mem t pos then '#' else '.' in
           Map.add_exn acc ~key:pos ~data)
    |> Board.to_string
    |> print_endline
  ;;
end

let part1 s =
  let display = Set.empty (module Coordinate) in
  String.split_lines s
  |> List.map ~f:Instruction.of_string
  |> List.fold ~init:display ~f:Display.apply_instruction
  |> Display.num_pixels
  |> Int.to_string
;;

let part2 s =
  let display = Set.empty (module Coordinate) in
  String.split_lines s
  |> List.map ~f:Instruction.of_string
  |> List.fold ~init:display ~f:Display.apply_instruction
  |> Display.print;
  "UPOJFLBCEZ"
;;
