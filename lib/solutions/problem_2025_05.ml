open! Core

type t =
  { ranges : (int * int) list
  ; ingredients : int list
  }

let is_fresh ranges ingredient =
  List.exists ranges ~f:(fun (low, high) -> Int.between ingredient ~low ~high)
;;

let count_total_fresh ranges =
  let min =
    List.map ranges ~f:fst |> List.min_elt ~compare:Int.compare |> Option.value_exn
  in
  let max =
    List.map ranges ~f:snd |> List.max_elt ~compare:Int.compare |> Option.value_exn
  in
  let rec solve acc cur =
    if cur > max
    then acc
    else (
      let next_start, next_end =
        List.fold
          ranges
          ~init:(Int.max_value, Int.min_value)
          ~f:(fun (next_start, next_end) (s, e) ->
            if e < cur || s >= next_start then next_start, next_end else Int.max cur s, e)
      in
      solve (acc + next_end - next_start + 1) (next_end + 1))
  in
  solve 0 min
;;

let parse s =
  String.split_lines s
  |> List.filter ~f:(fun s -> String.length s > 0)
  |> List.fold
       ~init:{ ranges = []; ingredients = [] }
       ~f:(fun { ranges; ingredients } line ->
         match String.split line ~on:'-' with
         | [ a; b ] ->
           { ranges = (Int.of_string a, Int.of_string b) :: ranges; ingredients }
         | a :: [] -> { ranges; ingredients = Int.of_string a :: ingredients }
         | _ -> raise_s [%message "bruh"])
;;

let part1 s =
  let t = parse s in
  List.count t.ingredients ~f:(is_fresh t.ranges) |> Int.to_string
;;

let part2 s =
  let t = parse s in
  count_total_fresh t.ranges |> Int.to_string
;;
