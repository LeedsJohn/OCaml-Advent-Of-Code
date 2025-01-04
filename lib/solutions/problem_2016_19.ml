open! Core

let get_next_idx nums idx = if idx + 1 >= Set.length nums then 0 else idx + 1
(* match Set.nth nums (idx + 1) with None -> 0 | Some res -> res *)
(* if idx + 1 >= Set.length nums then 0 else Set.nth nums (idx + 1) |> Option.value
  match
    Set.binary_search nums ~compare:Int.compare `First_strictly_greater_than
      prev
  with
  | Some res -> res
  | None -> Set.min_elt_exn nums  *)

let get_last start_elves =
  let rec aux nums cur =
    if Set.length nums = 1 then Set.nth nums 0 |> Option.value_exn
    else
      let eliminated_idx = get_next_idx nums cur in
      let nums = Set.remove_index nums eliminated_idx in
      aux nums (get_next_idx nums cur)
  in
  aux (Set.of_list (module Int) (List.range 1 (start_elves + 1))) 0

let get_last2 start_elves =
  let rec aux nums cur =
    if Set.length nums = 1 then Set.nth nums 0 |> Option.value_exn
    else
      let eliminated_idx = (cur + (Set.length nums / 2)) % Set.length nums in
      let nums = Set.remove_index nums eliminated_idx in
      let next =
        if eliminated_idx < cur then get_next_idx nums (cur - 1)
        else get_next_idx nums cur
      in
      aux nums next
  in
  aux (Set.of_list (module Int) (List.range 1 (start_elves + 1))) 0

let part1 s = get_last (Int.of_string s) |> Int.to_string |> Ok
let part2 s = get_last2 (Int.of_string s) |> Int.to_string |> Ok
