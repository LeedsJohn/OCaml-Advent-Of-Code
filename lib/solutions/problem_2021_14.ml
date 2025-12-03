open! Core

let step s insertion_rules =
  String.foldi s ~init:[] ~f:(fun i acc c ->
    if i + 1 = String.length s
    then c :: acc
    else (
      match Map.find insertion_rules (String.slice s i (i + 2)) with
      | None -> c :: acc
      | Some e -> e :: c :: acc))
  |> List.rev
  |> String.of_list
;;

let char_counts s =
  String.fold
    s
    ~init:(Map.empty (module Char))
    ~f:(fun acc c ->
      match Map.mem acc c with
      | true -> acc
      | false ->
        let count = String.count s ~f:(Char.equal c) in
        Map.add_exn acc ~key:c ~data:count)
;;

let merge_char_counts c1 c2 =
  Map.merge c1 c2 ~f:(fun ~key:_ elem ->
    match elem with
    | `Both (n1, n2) -> Some (n1 + n2)
    | `Left n | `Right n -> Some n)
;;

let memo = Hashtbl.create (module String)

let count_after_steps s rules steps =
  let rec aux s steps =
    let key = s ^ Int.to_string steps in
    match Hashtbl.find memo key with
    | Some res -> res
    | None ->
      let res =
        match steps with
        | 0 -> char_counts (String.slice s 0 1)
        | _ ->
          let next_s = step s rules in
          String.foldi
            next_s
            ~init:(Map.empty (module Char))
            ~f:(fun i acc _ ->
              if i + 1 = String.length next_s
              then acc
              else merge_char_counts acc (aux (String.slice next_s i (i + 2)) (steps - 1)))
      in
      Hashtbl.add_exn memo ~key ~data:res;
      res
  in
  aux s steps |> merge_char_counts (char_counts (String.suffix s 1))
;;

let parse s =
  let lines = String.split_lines s in
  let start = List.hd_exn lines in
  let insertion_rules =
    List.fold
      (List.tl_exn lines |> List.tl_exn)
      ~init:(Map.empty (module String))
      ~f:(fun acc line ->
        Map.add_exn acc ~key:(String.slice line 0 2) ~data:(String.get line 6))
  in
  start, insertion_rules
;;

let solve s steps =
  let start, rules = parse s in
  let counts = count_after_steps start rules steps |> Map.data in
  let big, little =
    List.fold counts ~init:(Int.min_value, Int.max_value) ~f:(fun (big, little) n ->
      Int.max big n, Int.min little n)
  in
  big - little
;;

let part1 s = Int.to_string (solve s 10)
let part2 s = Int.to_string (solve s 40)

let%expect_test "step" =
  let s =
    {|NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C|}
  in
  let start, rules = parse s in
  print_s [%sexp (step start rules : string)];
  [%expect {| NCNBCHB |}]
;;

let%expect_test "char counts" =
  let s =
    {|NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C|}
  in
  let show_stuff s =
    print_s [%sexp (String.length s : int)];
    let a = Map.to_alist (char_counts s) in
    print_s [%sexp (a : (char * int) list)]
  in
  let start, rules = parse s in
  show_stuff start;
  let _ =
    List.fold (List.range 0 10) ~init:start ~f:(fun s _ ->
      let s = step s rules in
      show_stuff s;
      s)
  in
  [%expect
    {|
    4
    ((B 1) (C 1) (N 2))
    7
    ((B 2) (C 2) (H 1) (N 2))
    13
    ((B 6) (C 4) (H 1) (N 2))
    25
    ((B 11) (C 5) (H 4) (N 5))
    49
    ((B 23) (C 10) (H 5) (N 11))
    97
    ((B 46) (C 15) (H 13) (N 23))
    193
    ((B 98) (C 31) (H 16) (N 48))
    385
    ((B 199) (C 48) (H 39) (N 99))
    769
    ((B 417) (C 96) (H 51) (N 205))
    1537
    ((B 845) (C 152) (H 118) (N 422))
    3073
    ((B 1749) (C 298) (H 161) (N 865))
    |}]
;;
