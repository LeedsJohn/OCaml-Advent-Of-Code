open! Core

let permutations l =
  let rec all_combos acc before after : ('a * 'a list) list =
    match after with
    | [] -> acc
    | hd :: tl -> all_combos ((hd, before @ tl) :: acc) (hd :: before) tl
  in
  let rec aux (l : 'a list) : 'a list list =
    match l with
    | [] -> []
    | [ hd ] -> [ [ hd ] ]
    | l ->
      all_combos [] [] l
      |> List.map ~f:(fun (n, l) ->
        aux l |> List.map ~f:(fun other_perms -> n :: other_perms))
      |> List.join
  in
  aux l
;;

let pop s i =
  let c = String.get s i in
  let res =
    if i = 0
    then String.drop_prefix s 1
    else if i + 1 = String.length s
    then String.drop_suffix s 1
    else String.slice s 0 i ^ String.slice s (i + 1) 0
  in
  c, res
;;

let insert s i c =
  let c = Char.to_string c in
  if i = 0
  then c ^ s
  else if i = String.length s
  then s ^ c
  else String.slice s 0 i ^ c ^ String.slice s i 0
;;

module Instruction = struct
  type t =
    | Swap_pos of int * int
    | Swap_char of char * char
    | Rotate_left of int
    | Rotate_right of int
    | Rotate_char of char
    | Rev of int * int
    | Move of int * int
  [@@deriving sexp_of]

  let swap_char s x y =
    String.map s ~f:(fun c ->
      if Char.equal x c then y else if Char.equal y c then x else c)
  ;;

  let swap_pos s x y = swap_char s (String.get s x) (String.get s y)

  let rev s x y =
    let beginning = if x = 0 then "" else String.slice s 0 x in
    let john = if y + 1 = String.length s then "" else String.slice s (y + 1) 0 in
    beginning ^ String.rev (String.slice s x (y + 1)) ^ john
  ;;

  let move s x y =
    let c, s = pop s x in
    insert s y c
  ;;

  let rotate_right s i =
    let n = String.length s in
    let i = i % n in
    if i = 0
    then s
    else (
      let start_pos = n - i in
      String.slice s start_pos 0 ^ String.slice s 0 start_pos)
  ;;

  let rotate_left s i = rotate_right s (String.length s - i)

  let rotate_char s c =
    let i = String.substr_index_exn s ~pattern:(Char.to_string c) in
    let num_rotates = 1 + i + if i >= 4 then 1 else 0 in
    rotate_right s num_rotates
  ;;

  let apply_instruction s = function
    | Swap_pos (x, y) -> swap_pos s x y
    | Swap_char (x, y) -> swap_char s x y
    | Rev (x, y) -> rev s x y
    | Move (x, y) -> move s x y
    | Rotate_left x -> rotate_left s x
    | Rotate_right x -> rotate_right s x
    | Rotate_char c -> rotate_char s c
  ;;

  let of_string s =
    let words = String.split s ~on:' ' |> List.to_array in
    match words.(0) with
    | "reverse" -> Rev (Int.of_string words.(2), Int.of_string words.(4))
    | "move" -> Move (Int.of_string words.(2), Int.of_string words.(5))
    | "swap" ->
      (match words.(1) with
       | "position" -> Swap_pos (Int.of_string words.(2), Int.of_string words.(5))
       | _ -> Swap_char (Char.of_string words.(2), Char.of_string words.(5)))
    | "rotate" ->
      (match words.(1) with
       | "left" -> Rotate_left (Int.of_string words.(2))
       | "right" -> Rotate_right (Int.of_string words.(2))
       | _ -> Rotate_char (Char.of_string words.(6)))
    | s -> raise_s [%message "bad instruction" ~instruction:(s : string)]
  ;;

  let scramble s instructions =
    List.fold instructions ~init:s ~f:(fun before i ->
      let after = apply_instruction before i in
      (* if
          Set.of_list (module Char) (String.to_list after)
          |> Set.length <> String.length after
        then raise_s [%message "it's messed up" (after : string)]; *)
      after)
  ;;
end

let get_instructions s = String.split_lines s |> List.map ~f:Instruction.of_string

let part1 s =
  let instructions = get_instructions s in
  Instruction.scramble "abcdefgh" instructions
;;

let part2 s =
  let instructions = get_instructions s in
  let all_strings =
    permutations (String.to_list "abcdefgh") |> List.map ~f:String.of_list
  in
  List.find_exn all_strings ~f:(fun s ->
    String.equal (Instruction.scramble s instructions) "fbgdceah")
;;

let%expect_test "" =
  print_s [%sexp (Instruction.rotate_right "abcd" 1 : string)];
  [%expect {| dabc |}];
  print_s [%sexp (Instruction.rotate_right "abcd" 2 : string)];
  [%expect {| cdab |}];
  print_s [%sexp (Instruction.rotate_left "abcd" 1 : string)];
  [%expect {| bcda |}];
  print_s [%sexp (Instruction.rotate_left "abcd" 2 : string)];
  [%expect {| cdab |}];
  ()
;;
