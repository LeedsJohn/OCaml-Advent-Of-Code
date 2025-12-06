open! Core

let get_dig n i = n / Int.pow 10 i % 10

let verify n =
  let has_repeat =
    List.range 1 6 |> List.exists ~f:(fun i -> get_dig n i = get_dig n (i - 1))
  in
  let not_decreasing =
    List.range 1 6 |> List.for_all ~f:(fun i -> get_dig n (i - 1) >= get_dig n i)
  in
  has_repeat && not_decreasing
;;

let verify2 n =
  let s = Int.to_string n in
  let bruh =
    List.range 0 10
    |> List.map ~f:Int.to_string
    |> List.exists ~f:(fun c ->
      String.is_substring s ~substring:(c ^ c)
      && not (String.is_substring s ~substring:(c ^ c ^ c)))
  in
  verify n && bruh
;;

let part1 s =
  let low, high =
    match String.split s ~on:'-' with
    | [ a; b ] -> Int.of_string a, Int.of_string b
    | l -> raise_s [%message "this failed" (l : string list)]
  in
  List.range low (high + 1) |> List.count ~f:verify |> Int.to_string
;;

let part2 s =
  let low, high =
    match String.split s ~on:'-' with
    | [ a; b ] -> Int.of_string a, Int.of_string b
    | l -> raise_s [%message "this failed" (l : string list)]
  in
  List.range low (high + 1) |> List.count ~f:verify2 |> Int.to_string
;;
