open! Core
open Helpers

let part1 lines =
  String.split_lines lines
  |> List.map ~f:(fun line ->
         (if Char.equal 'L' (String.get line 0) then -1 else 1)
         * fst (Parse.take_next_int line))
  |> List.fold ~init:(50, 0) ~f:(fun (pos, count) cur ->
         let next = (pos + cur) % 100 in
         print_s [%sexp (next : int)];
         (next, count + Bool.to_int (pos = 0)))
  |> snd |> Int.to_string |> Ok

let part2 lines =
  String.split_lines lines
  |> List.map ~f:(fun line ->
         (if Char.equal 'L' (String.get line 0) then -1 else 1)
         * fst (Parse.take_next_int line))
  |> List.fold ~init:(50, 0) ~f:(fun (pos, count) cur ->
         let count = count + (Int.abs cur / 100) in
         let extra_click =
           let extra = if cur < 0 then -(-cur % 100) else cur % 100 in
           pos <> 0 && (pos + extra >= 100 || pos + extra <= 0)
         in
         let next = (pos + cur) % 100 in
         let count = count + Bool.to_int extra_click in
         (next, count))
  |> snd |> Int.to_string |> Ok
