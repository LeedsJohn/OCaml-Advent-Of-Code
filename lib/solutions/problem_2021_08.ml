open! Core

let digits =
  Map.of_alist_exn
    (module String)
    [ "abcefg", 0
    ; "cf", 1
    ; "acdeg", 2
    ; "acdfg", 3
    ; "bcdf", 4
    ; "abdfg", 5
    ; "abdefg", 6
    ; "acf", 7
    ; "abcdefg", 8
    ; "abcdfg", 9
    ]
;;

module Mapping = struct
  type t = (char, char) List.Assoc.t

  let create s : t = List.zip_exn (String.to_list s) (String.to_list "abcdefg")

  let convert t number =
    String.map number ~f:(fun c -> List.Assoc.find_exn t ~equal:Char.equal c)
    |> String.to_list
    |> List.sort ~compare:Char.compare
    |> String.of_list
  ;;
end

let try_mapping numbers mapping =
  List.for_all numbers ~f:(fun number -> Map.mem digits (Mapping.convert mapping number))
;;

let translate number mapping = Map.find_exn digits (Mapping.convert mapping number)

let thing numbers mapping =
  List.map numbers ~f:(fun num -> translate num mapping)
  |> List.map ~f:(fun n -> Int.to_string n |> Char.of_string)
  |> String.of_list
  |> Int.of_string
;;

let get_mapping samples =
  let stuff = Set.of_list (module Char) (String.to_list "abcdefg") in
  let rec aux (mapping : string) (chars : Set.M(Char).t) : Mapping.t option =
    match Set.length chars with
    | 0 ->
      let m = Mapping.create mapping in
      if try_mapping samples m then Some m else None
    | _ ->
      Set.find_map chars ~f:(fun c ->
        aux (mapping ^ Char.to_string c) (Set.remove chars c))
  in
  aux "" stuff |> Option.value_exn
;;

let get_lines s =
  let split_line l =
    String.split l ~on:' ' |> List.filter ~f:(fun s -> String.length s >= 2)
  in
  String.split_lines s
  |> List.map ~f:(fun line ->
    match String.split line ~on:'|' with
    | [ a; b ] -> split_line a, split_line b
    | _ -> raise_s [%message "malformed line" ~line:(line : string)])
;;

let part1 _ = raise_s [%message "unimplemented"]

let part2 s =
  let lines = get_lines s in
  List.fold lines ~init:0 ~f:(fun acc (sample, stuff) ->
    let mapping = get_mapping sample in
    acc + thing stuff mapping)
  |> Int.to_string
;;

let%expect_test "translating" =
  let m = Mapping.create "deafgbc" in
  List.iter
    [ "cagedb"
    ; "ab"
    ; "gcdfa"
    ; "fbcad"
    ; "eafb"
    ; "cdfbe"
    ; "cdfgeb"
    ; "dab"
    ; "acedfgb"
    ; "cefabd"
    ]
    ~f:(fun num -> print_s [%sexp (translate num m : int)]);
  [%expect
    {|
      0
      1
      2
      3
      4
      5
      6
      7
      8
      9
      |}]
;;

let%expect_test "parsing" =
  let s =
    "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
  in
  let lines = get_lines s in
  print_s [%sexp (lines : (string list * string list) list)];
  [%expect
    {|
      (((acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab)
        (cdfeb fcadb cdfeb cdbaf)))
      |}]
;;
