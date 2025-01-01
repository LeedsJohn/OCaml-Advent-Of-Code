open! Core

let presents_delivered n =
  let res = ref 0 in
  for i = 1 to n + 1 do
    if n % i = 0 then res := !res + i
  done;
  10 * !res

let presents_delivered2 n =
  let res = ref 0 in
  let min_value = if n % 50 = 0 then n / 50 else (n / 50) + 1 in
  for i = Int.max 1 min_value to n + 1 do
    if n % i = 0 then res := !res + i
  done;
  11 * !res

let part1 s =
  let n = Int.of_string s in
  let rec aux i =
    if i % 1000 = 0 then print_s [%sexp (i : int)];
    if presents_delivered i >= n then i else aux (i + 1)
  in
  aux 1 |> Int.to_string |> Ok

let part2 s =
  let n = Int.of_string s in
  let rec aux i =
    if i % 1000 = 0 then print_s [%sexp (i : int)];
    if presents_delivered2 i >= n then i else aux (i + 1)
  in
  aux 1 |> Int.to_string |> Ok

let%expect_test "" =
  print_s [%sexp (presents_delivered 8 : int)];
  [%expect {| 150 |}];
  print_s [%sexp (presents_delivered2 51 : int)];
  [%expect {| 781 |}];
  ()
