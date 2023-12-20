open Core

let day = 19
let year = 2023

type part = { x : int; m : int; a : int; s : int }

let sum_part part = part.x + part.m + part.a + part.s

type rule = { f : part -> bool; dest : string }

let parse_text text =
  let double_line_char = Char.of_int_exn 0 in
  let sections =
    String.strip text
    |> String.substr_replace_all ~pattern:"\n\n"
         ~with_:(String.of_char double_line_char)
    |> String.split ~on:double_line_char
  in
  let rules, parts = (List.nth_exn sections 0, List.nth_exn sections 1) in
  let make_rule r =
    if not (String.contains r ':') then { f = (fun _ -> true); dest = r }
    else
      let r = String.split r ~on:':' in
      let comparison, dest = (List.nth_exn r 0, List.nth_exn r 1) in
      let num =
        String.slice comparison 2 (String.length comparison) |> Int.of_string
      in
      let f =
        match (String.get comparison 0, String.get comparison 1) with
        | 'x', '<' -> fun part -> part.x < num
        | 'x', '>' -> fun part -> part.x > num
        | 'm', '<' -> fun part -> part.m < num
        | 'm', '>' -> fun part -> part.m > num
        | 'a', '<' -> fun part -> part.a < num
        | 'a', '>' -> fun part -> part.a > num
        | 's', '<' -> fun part -> part.s < num
        | _, _ -> fun part -> part.s > num
      in
      { f; dest }
  in
  let make_part part_text =
    let part_text = String.slice part_text 1 (String.length part_text - 1) in
    let values = String.split part_text ~on:',' in
    let extract_number s =
      String.slice s 2 (String.length s) |> Int.of_string
    in
    let x = extract_number (List.nth_exn values 0) in
    let m = extract_number (List.nth_exn values 1) in
    let a = extract_number (List.nth_exn values 2) in
    let s = extract_number (List.nth_exn values 3) in
    { x; m; a; s }
  in
  let rules =
    List.fold (String.split_lines rules)
      ~init:(Map.empty (module String))
      ~f:(fun acc rule_text ->
        let start_rules_index = String.index_exn rule_text '{' in
        let name = String.slice rule_text 0 start_rules_index in
        let rule_text =
          String.slice rule_text (start_rules_index + 1)
            (String.length rule_text - 1)
        in
        let rules = List.map (String.split rule_text ~on:',') ~f:make_rule in
        Map.add_exn acc ~key:name ~data:rules)
  in
  let parts = List.map (String.split_lines parts) ~f:make_part in
  (rules, parts)

let get_dest rules part =
  let first_rule = List.find_exn rules ~f:(fun rule -> rule.f part) in
  first_rule.dest

let is_accepted rules_map part =
  let rec aux location =
    match location with
    | "A" -> true
    | "R" -> false
    | _ -> aux (get_dest (Map.find_exn rules_map location) part)
  in
  aux "in"

let part1 fname =
  let rules, parts = parse_text (In_channel.read_all fname) in
  List.fold parts ~init:0 ~f:(fun acc part ->
      if is_accepted rules part then acc + sum_part part else acc)
  |> Int.to_string

type comparison = Any | At_least | At_most [@@deriving sexp]

type part_range = { x : int * int; m : int * int; a : int * int; s : int * int }
[@@deriving sexp]

let num_in_range part =
  let get_num (little, big) = big - little + 1 in
  get_num part.x * get_num part.m * get_num part.a * get_num part.s

type rule_range = {
  comp : comparison;
  num : int;
  category : char;
  dest : string;
}
[@@deriving sexp]

let update_part_range (part : part_range) (cat : char) (range : int * int) :
    part_range =
  match cat with
  | 'x' -> { part with x = range }
  | 'm' -> { part with m = range }
  | 'a' -> { part with a = range }
  | _ -> { part with s = range }

let get_filtered_parts rule part =
  let min_num, max_num =
    match rule.category with
    | 'x' -> part.x
    | 'm' -> part.m
    | 'a' -> part.a
    | _ -> part.s
  in
  let accepted =
    match rule.comp with
    | Any -> Some part
    | At_least ->
        if rule.num > max_num then None
        else
          Some
            (update_part_range part rule.category
               (Int.max rule.num min_num, max_num))
    | At_most ->
        if rule.num < min_num then None
        else
          Some
            (update_part_range part rule.category
               (min_num, Int.min rule.num max_num))
  in
  let rejected =
    match rule.comp with
    | Any -> None
    | At_least ->
        if min_num >= rule.num then None
        else
          Some
            (update_part_range part rule.category
               (min_num, Int.min (rule.num - 1) max_num))
    | At_most ->
        if max_num <= rule.num then None
        else
          Some
            (update_part_range part rule.category
               (Int.max (rule.num + 1) min_num, max_num))
  in
  (accepted, rejected)

let parse_text2 text =
  let double_line_char = Char.of_int_exn 0 in
  let rules =
    String.strip text
    |> String.substr_replace_all ~pattern:"\n\n"
         ~with_:(String.of_char double_line_char)
    |> String.split ~on:double_line_char
    |> List.hd_exn |> String.split_lines
  in
  let make_rule_range r =
    if not (String.contains r ':') then
      { comp = Any; num = -1; category = ' '; dest = r }
    else
      let r = String.split r ~on:':' in
      let comparison, dest = (List.nth_exn r 0, List.nth_exn r 1) in
      let num =
        String.slice comparison 2 (String.length comparison) |> Int.of_string
      in
      let comp, num =
        if Char.(String.get comparison 1 = '<') then (At_most, num - 1)
        else (At_least, num + 1)
      in
      let category = String.get comparison 0 in
      { comp; num; category; dest }
  in
  List.fold rules
    ~init:(Map.empty (module String))
    ~f:(fun acc rule_text ->
      let start_rules_index = String.index_exn rule_text '{' in
      let name = String.slice rule_text 0 start_rules_index in
      let rule_text =
        String.slice rule_text (start_rules_index + 1)
          (String.length rule_text - 1)
      in
      let rules =
        List.map (String.split rule_text ~on:',') ~f:make_rule_range
      in
      Map.add_exn acc ~key:name ~data:rules)

let part2 fname =
  let rules = parse_text2 (In_channel.read_all fname) in
  let rec aux location part =
    match location with
    | "A" -> num_in_range part
    | "R" -> 0
    | _ ->
        let res, _ =
          List.fold (Map.find_exn rules location) ~init:(0, Some part)
            ~f:(fun (num, part) rule ->
              match part with
              | None -> (num, part)
              | Some part ->
                  let accepted, rejected = get_filtered_parts rule part in
                  let num =
                    num
                    +
                    match accepted with
                    | None -> 0
                    | Some accepted -> aux rule.dest accepted
                  in
                  (num, rejected))
        in
        res
  in
  aux "in" { x = (1, 4000); m = (1, 4000); a = (1, 4000); s = (1, 4000) }
  |> Int.to_string

let%expect_test "range stuff" =
  let initial_range =
    { x = (1, 100); m = (1, 100); a = (1, 100); s = (1, 100) }
  in
  let rule = { comp = At_least; num = 50; category = 'x'; dest = "" } in
  print_s
    [%sexp
      (get_filtered_parts rule initial_range
        : part_range option * part_range option)];
  [%expect {|
    ((((x (50 100)) (m (1 100)) (a (1 100)) (s (1 100))))
     (((x (1 49)) (m (1 100)) (a (1 100)) (s (1 100))))) |}];
  let rule = { comp = At_most; num = 50; category = 'x'; dest = "" } in
  print_s
    [%sexp
      (get_filtered_parts rule initial_range
        : part_range option * part_range option)];
  [%expect
    {|
    ((((x (1 50)) (m (1 100)) (a (1 100)) (s (1 100))))
     (((x (51 100)) (m (1 100)) (a (1 100)) (s (1 100))))) |}];
  ()
