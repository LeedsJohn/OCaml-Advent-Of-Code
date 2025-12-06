open! Core
open Helpers

(* i thought this would help me with the second part but no such luck *)
let to_matrix s =
  let lines = String.split_lines s in
  let numbers = List.rev lines |> List.tl_exn |> List.rev in
  let ops = List.rev lines |> List.hd_exn in
  List.map numbers ~f:(Fn.compose (List.map ~f:Int.to_string) Parse.line_numbers)
  @ [ String.filter ops ~f:(Fn.compose not Char.is_whitespace)
      |> String.to_list
      |> List.map ~f:Char.to_string
    ]
  |> List.map ~f:List.to_array
  |> List.to_array
;;

let s_to_f = function
  | "*" -> Int.( * )
  | "+" -> Int.( + )
  | s -> raise_s [%message "bad function string" (s : string)]
;;

let solve_col mat col =
  let rows = Array.length mat in
  let f = s_to_f mat.(rows - 1).(col) in
  List.range 1 (rows - 1)
  |> List.fold
       ~init:(Int.of_string mat.(0).(col))
       ~f:(fun acc row -> f acc (Int.of_string mat.(row).(col)))
;;

let part1 s =
  let mat = to_matrix s in
  List.range 0 (Array.length mat.(0))
  |> List.sum (module Int) ~f:(solve_col mat)
  |> Int.to_string
;;

let get_equation_starts s =
  String.split_lines s
  |> List.rev
  |> List.hd_exn
  |> String.to_list
  |> List.mapi ~f:(fun i c -> c, i)
  |> List.filter ~f:(fun (c, _i) -> not (Char.is_whitespace c))
  |> List.map ~f:snd
;;

let solve_col2 lines start end_ =
  let rows = Array.length lines in
  let nums =
    List.range start end_
    |> List.map ~f:(fun col ->
      List.range ~stride:(-1) (rows - 2) (-1)
      |> List.fold ~init:(0, 1) ~f:(fun (acc, pow) row ->
        match String.get lines.(row) col with
        | ' ' -> acc, pow
        | c -> acc + ((Char.to_int c - Char.to_int '0') * pow), pow * 10)
      |> fst)
  in
  let f = s_to_f (String.get lines.(rows - 1) start |> Char.to_string) in
  List.fold (List.tl_exn nums) ~init:(List.hd_exn nums) ~f
;;

(* this problem was kinda lame imo, mostly string parsing. i also think that it wasn't
   well specified, but that's kinda part of aoc. mostly i'm just mad i got skill issued
   and it took me longer than it should have *)
let part2 s =
  let lines = String.split_lines s |> List.to_array in
  let equation_starts = get_equation_starts s in
  List.zip_exn
    equation_starts
    (List.tl_exn equation_starts @ [ String.length lines.(0) + 1 ])
  |> List.sum (module Int) ~f:(fun (start, end_) -> solve_col2 lines start (end_ - 1))
  |> Int.to_string
;;
