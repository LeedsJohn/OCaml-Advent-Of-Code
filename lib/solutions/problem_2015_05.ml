open! Core

let contains_n_vowels s n =
  let num_vowels = String.count s ~f:(fun c -> String.mem "aeiou" c) in
  num_vowels >= n

let has_repeating s =
  String.existsi (String.drop_suffix s 1) ~f:(fun i c ->
      Char.equal c (String.get s (i + 1)))

let contains_pattern s patterns =
  List.exists patterns ~f:(fun substring -> String.is_substring s ~substring)

let is_nice_string s =
  (not (contains_pattern s [ "ab"; "cd"; "pq"; "xy" ]))
  && contains_n_vowels s 3 && has_repeating s

let pattern_appears_twice s pattern =
  String.substr_index_all s ~may_overlap:false ~pattern |> List.length >= 2

let has_double_thing s =
  String.existsi (String.drop_suffix s 1) ~f:(fun i _ ->
      pattern_appears_twice s (String.slice s i (i + 2)))

let has_repeating_with_between s =
  String.existsi (String.drop_suffix s 2) ~f:(fun i c ->
      Char.equal c (String.get s (i + 2)))

let is_nice_string2 s = has_double_thing s && has_repeating_with_between s

let part1 s =
  String.split_lines s |> List.count ~f:is_nice_string |> Int.to_string |> Ok

let part2 s =
  String.split_lines s |> List.count ~f:is_nice_string2 |> Int.to_string |> Ok
