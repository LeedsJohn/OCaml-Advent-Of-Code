open! Core

let max_length = 8

type node =
  | Empty
  | End
  | Part

let trie = Array.create ~len:(Int.pow 6 max_length) Empty

let color_to_int = function
  | 'w' -> 1
  | 'u' -> 2
  | 'b' -> 3
  | 'r' -> 4
  | 'g' -> 5
  | color -> raise_s [%message "bad color character" (color : char)]
;;

module Trie = struct
  let add s =
    let end_pos =
      String.foldi s ~init:0 ~f:(fun i pos c ->
        let new_pos = pos + (Int.pow 6 i * color_to_int c) in
        (match trie.(new_pos) with
         | Empty -> trie.(new_pos) <- Part
         | _ -> ());
        new_pos)
    in
    trie.(end_pos) <- End
  ;;

  let get_valid_end_indices s start_i =
    let rec aux acc i pos =
      if start_i + i >= String.length s
      then acc
      else (
        let new_pos = pos + (Int.pow 6 i * color_to_int (String.get s (start_i + i))) in
        if new_pos < Array.length trie
        then (
          match trie.(new_pos) with
          | Empty -> acc
          | Part -> aux acc (i + 1) new_pos
          | End -> aux ((start_i + i + 1) :: acc) (i + 1) new_pos)
        else acc)
    in
    aux [] 0 0
  ;;
end

let of_string s =
  String.take_while s ~f:(fun c -> not (Char.equal c '\n'))
  |> String.filter ~f:(fun c -> not (Char.is_whitespace c))
  |> String.split ~on:','
  |> List.iter ~f:Trie.add;
  let lines = String.split_lines s in
  List.drop lines 2
;;

let can_make s =
  let rec aux i =
    if i >= String.length s
    then true
    else List.exists (Trie.get_valid_end_indices s i) ~f:aux
  in
  aux 0
;;

let ways_to_make s =
  let cache = Array.create ~len:(String.length s) (-1) in
  let rec aux i =
    if i >= String.length s
    then 1
    else if cache.(i) <> -1
    then cache.(i)
    else (
      let res = List.sum (module Int) (Trie.get_valid_end_indices s i) ~f:aux in
      cache.(i) <- res;
      res)
  in
  aux 0
;;

let part1 s =
  let towels = of_string s in
  List.count towels ~f:can_make |> Int.to_string
;;

let part2 s =
  let towels = of_string s in
  List.sum (module Int) towels ~f:ways_to_make |> Int.to_string
;;

let reset_trie () = Array.fill trie ~pos:0 ~len:(Array.length trie) Empty

let test_string =
  {|r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb|}
;;

let%expect_test "" =
  reset_trie ();
  let _towels = of_string test_string in
  print_s [%sexp (Trie.get_valid_end_indices "brwrr" 0 : int list)];
  [%expect {| (2 1) |}];
  print_s [%sexp (Trie.get_valid_end_indices "brwrr" 2 : int list)];
  [%expect {| (4) |}];
  print_s [%sexp (Trie.get_valid_end_indices "brwrr" 4 : int list)];
  [%expect {| (5) |}];
  print_s [%sexp (can_make "brwrr" : bool)];
  [%expect {| true |}];
  ()
;;
