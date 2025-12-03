open! Core

let can_make_sum nums goal =
  let rec aux cur nums =
    if cur = goal
    then true
    else if cur > goal
    then false
    else (
      match nums with
      | [] -> false
      | hd :: tl -> aux (cur + hd) tl || aux cur tl)
  in
  aux 0 nums
;;

let can_partition_3 nums goal =
  let rec aux acc cur_sum before after =
    if cur_sum = goal
    then can_make_sum (before @ after) goal
    else if cur_sum > goal
    then false
    else (
      match after with
      | [] -> false
      | hd :: tl ->
        aux (hd :: acc) (hd + cur_sum) before tl || aux acc cur_sum (hd :: before) tl)
  in
  aux [] 0 [] nums
;;

let min_packages_to_make nums goal =
  let nums = List.sort nums ~compare:(fun n1 n2 -> Int.compare n2 n1) in
  let rec aux acc count nums =
    if acc > goal
    then Int.max_value
    else if acc = goal
    then count
    else (
      match nums with
      | [] -> Int.max_value
      | hd :: tl -> Int.min (aux (acc + hd) (count + 1) tl) (aux acc count tl))
  in
  aux 0 0 nums
;;

let get_best_thing ?(check_fn = can_make_sum) nums goal =
  let qe nums = List.fold nums ~init:1 ~f:Int.( * ) in
  let min_count = min_packages_to_make nums goal in
  let rec aux acc cur_sum before after =
    if cur_sum = goal
    then if check_fn (before @ after) goal then qe acc else Int.max_value
    else if cur_sum > goal || List.length acc >= min_count
    then Int.max_value
    else (
      match after with
      | [] -> Int.max_value
      | hd :: tl ->
        Int.min
          (aux (hd :: acc) (cur_sum + hd) before tl)
          (aux acc cur_sum (hd :: before) tl))
  in
  aux [] 0 [] nums
;;

let get_nums s = String.split_lines s |> List.map ~f:Int.of_string

let part1 s =
  let nums = get_nums s in
  let goal = List.sum (module Int) nums ~f:Fn.id / 3 in
  get_best_thing nums goal |> Int.to_string
;;

let part2 s =
  let nums = get_nums s in
  let goal = List.sum (module Int) nums ~f:Fn.id / 4 in
  get_best_thing ~check_fn:can_partition_3 nums goal |> Int.to_string
;;
