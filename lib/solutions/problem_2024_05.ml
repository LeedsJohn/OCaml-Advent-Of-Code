open! Core
open! Helpers

let parse s =
  let rec get_order_rules acc = function
    | "" :: tl -> (acc, tl)
    | line :: tl ->
        let num1, s = Parse.take_int line in
        let num2, _ = Parse.take_int (String.drop_prefix s 1) in
        get_order_rules (Set.add acc (num1, num2)) tl
    | _ -> raise_s [%message "john"]
  in
  let order_rules, lines =
    get_order_rules (Set.empty (module Coordinate)) (String.split_lines s)
  in
  let updates =
    List.map lines ~f:(fun line ->
        String.split line ~on:',' |> List.map ~f:Int.of_string)
  in
  (order_rules, updates)

let valid_update order_rules update =
  let rec get_pairs acc = function
    | _ :: [] | [] -> acc
    | hd :: tl -> get_pairs (acc @ List.map tl ~f:(fun n -> (hd, n))) tl
  in
  let pairs = get_pairs [] update in
  List.for_all pairs ~f:(fun (first, second) ->
      not (Set.mem order_rules (second, first)))

let fix_update order_rules update =
  (* change the update line to a set of numbers. While it isn't empty, find a number
     that doesn't have to go behind any other number in the remaining numbers. Add
     that one to the result and remove it from the set of remaining numbers. *)
  let rec aux acc update =
    if Set.length update = 0 then List.rev acc
    else
      let next =
        Set.find_exn update ~f:(fun n1 ->
            Set.exists update ~f:(fun n2 -> Set.mem order_rules (n2, n1)) |> not)
      in
      aux (next :: acc) (Set.remove update next)
  in
  aux [] (Set.of_list (module Int) update)

let part1 s =
  let order_rules, updates = parse s in
  List.sum
    (module Int)
    updates
    ~f:(fun update ->
      if valid_update order_rules update then
        List.nth_exn update (List.length update / 2)
      else 0)
  |> Int.to_string |> Ok

let part2 s =
  let order_rules, updates = parse s in
  List.filter updates ~f:(fun update -> not (valid_update order_rules update))
  |> List.map ~f:(fix_update order_rules)
  |> List.sum
       (module Int)
       ~f:(fun update -> List.nth_exn update (List.length update / 2))
  |> Int.to_string |> Ok
