open! Core
open! Helpers

let sort_rings rings =
  List.sort rings ~compare:(fun (c1, _, _) (c2, _, _) -> Int.compare c1 c2)
;;

let weapons = [ 8, 4, 0; 10, 5, 0; 25, 6, 0; 40, 7, 0; 74, 8, 0 ]
let armor = [ 0, 0, 0; 13, 0, 1; 31, 0, 2; 53, 0, 3; 75, 0, 4; 102, 0, 5 ]
let one_ring = [ 25, 1, 0; 50, 2, 0; 100, 3, 0; 20, 0, 1; 40, 0, 2; 80, 0, 3 ]
let eq (a1, b1, c1) (a2, b2, c2) = a1 = a2 && b1 = b2 && c1 = c2

let two_ring_combos =
  List.cartesian_product one_ring one_ring
  |> List.filter ~f:(fun (r1, r2) -> not (eq r1 r2))
;;

let rings =
  [ 0, 0, 0 ]
  @ one_ring
  @ List.map two_ring_combos ~f:(fun ((a1, b1, c1), (a2, b2, c2)) ->
    a1 + a2, b1 + b2, c1 + c2)
;;

let all_combos =
  List.cartesian_product weapons armor
  |> List.cartesian_product rings
  |> List.map ~f:(fun ((a1, b1, c1), ((a2, b2, c2), (a3, b3, c3))) ->
    a1 + a2 + a3, b1 + b2 + b3, c1 + c2 + c3)
  |> sort_rings
;;

let do_i_win my_dam my_armor hp dam armor =
  let get_turns hp attack =
    let turns = hp / attack in
    if turns * attack >= hp then turns else turns + 1
  in
  let d = Int.max 1 (my_dam - armor) in
  let my_turns = get_turns hp d in
  let his_d = Int.max 1 (dam - my_armor) in
  let his_turns = get_turns 100 his_d in
  my_turns <= his_turns
;;

let get_stats s =
  match String.split_lines s with
  | [ a; b; c ] ->
    let hp, _ = Parse.take_next_int a in
    let d, _ = Parse.take_next_int b in
    let a, _ = Parse.take_next_int c in
    hp, d, a
  | _ -> raise_s [%message "bad number of lines"]
;;

let part1 s =
  let hp, d, a = get_stats s in
  List.find_map_exn all_combos ~f:(fun (c, my_dam, my_armor) ->
    if do_i_win my_dam my_armor hp d a then Some c else None)
  |> Int.to_string
;;

let part2 s =
  let hp, d, a = get_stats s in
  print_s [%sexp (List.hd_exn (List.rev all_combos) : int * int * int)];
  List.rev all_combos
  |> List.find_map_exn ~f:(fun (c, my_dam, my_armor) ->
    if not (do_i_win my_dam my_armor hp d a) then Some c else None)
  |> Int.to_string
;;
