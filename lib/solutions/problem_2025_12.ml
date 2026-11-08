open! Core
open Helpers

(* this problem sucks *)

let parse s =
  let lines = String.split_lines s in
  let shapes = Hashtbl.create (module Int) in
  let rec aux i = function
    | s :: a :: b :: c :: _ :: tl ->
      if not (String.mem s 'x')
      then (
        Hashtbl.add_exn shapes ~key:i ~data:(String.count (a ^ b ^ c) ~f:(Char.equal '#'));
        aux (i + 1) tl)
    | _ -> ()
  in
  aux 0 lines;
  let borders =
    List.filter lines ~f:(fun line -> String.mem line 'x')
    |> List.map ~f:Parse.line_numbers
  in
  shapes, borders
;;

let shit_helper shapes border =
  let area, shape_counts =
    match border with
    | a :: b :: tl -> a * b, tl
    | _ -> failwith ""
  in
  area
  >= (List.mapi shape_counts ~f:(fun i n -> n * Hashtbl.find_exn shapes i)
      |> List.sum (module Int) ~f:Fn.id)
;;

let shit shapes borders = List.count borders ~f:(shit_helper shapes)

let part1 s =
  let shapes, borders = parse s in
  shit shapes borders |> Int.to_string
;;

let part2 _ = raise_s [%message "unimplemented"]
