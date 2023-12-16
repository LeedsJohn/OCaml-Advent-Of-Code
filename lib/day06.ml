open Core

let day = 6
let year = 2023
let is_numeric s = String.length s > 0 && String.for_all s ~f:Char.is_digit

let parse_input1 fname =
  let races =
    In_channel.read_lines fname
    |> List.map ~f:(fun line ->
           List.fold (String.split line ~on:' ') ~init:[] ~f:(fun acc num ->
               if is_numeric num then Int.of_string num :: acc else acc)
           |> List.rev)
  in
  List.zip_exn (List.hd_exn races) (List.hd_exn (List.tl_exn races))

let parse_input2 fname =
  let race =
    In_channel.read_lines fname
    |> List.map ~f:(fun line ->
           List.fold (String.split line ~on:' ') ~init:"" ~f:(fun acc num ->
               if is_numeric num then acc ^ num else acc)
           |> Int.of_string)
  in
  (List.hd_exn race, List.hd_exn (List.tl_exn race))

let get_distance time wait = wait * (time - wait)

let get_min_wait time dist =
  let rec b_search l r =
    if l < 0 || r > time || l > r then None
    else
      let m = (l + r) / 2 in
      match (get_distance time m > dist, get_distance time (m - 1) > dist) with
      | true, false -> Some m
      | _, true -> b_search l (m - 1)
      | false, false ->
          let a = b_search l (m - 1) in
          if Option.is_some a then a else b_search (m + 1) r
  in
  b_search 1 (time - 1)

let get_max_wait time dist =
  let rec b_search l r =
    if l < 0 || r > time || l > r then None
    else
      let m = (l + r) / 2 in
      match (get_distance time m > dist, get_distance time (m - 1) > dist) with
      | false, true -> Some (m - 1)
      | true, _ -> b_search (m + 1) r
      | false, false ->
          let a = b_search l (m - 1) in
          if Option.is_some a then a else b_search (m + 1) r
  in
  b_search 1 (time - 1)

let part1 fname =
  let races = parse_input1 fname in
  List.fold races ~init:1 ~f:(fun acc (time, dist) ->
      match (get_min_wait time dist, get_max_wait time dist) with
      | Some min, Some max -> acc * (max - min + 1)
      | _, _ -> acc)
  |> Int.to_string

let part2 fname =
  let time, dist = parse_input2 fname in
  let min = get_min_wait time dist |> Option.value_exn in
  let max = get_max_wait time dist |> Option.value_exn in
  max - min + 1 |> Int.to_string
