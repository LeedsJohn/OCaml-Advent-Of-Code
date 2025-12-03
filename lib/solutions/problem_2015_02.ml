open! Core
open! Helpers

let of_string s =
  String.split_lines s
  |> List.map ~f:(fun line ->
    let l, line = Parse.take_next_int line in
    let w, line = Parse.take_next_int line in
    let h, _ = Parse.take_next_int line in
    l, w, h)
;;

let get_required_paper l w h =
  let smallest_side =
    let longest = Int.max l w |> Int.max h in
    l * w * h / longest
  in
  (2 * l * w) + (2 * w * h) + (2 * h * l) + smallest_side
;;

let get_required_ribbon l w h =
  let l, w, h =
    match List.sort [ l; w; h ] ~compare:Int.compare with
    | [ a; b; c ] -> a, b, c
    | _ -> raise_s [%message "wtf"]
  in
  (2 * l) + (2 * w) + (l * w * h)
;;

let part1 s =
  of_string s
  |> List.sum (module Int) ~f:(fun (l, w, h) -> get_required_paper l w h)
  |> Int.to_string
;;

let part2 s =
  of_string s
  |> List.sum (module Int) ~f:(fun (l, w, h) -> get_required_ribbon l w h)
  |> Int.to_string
;;
