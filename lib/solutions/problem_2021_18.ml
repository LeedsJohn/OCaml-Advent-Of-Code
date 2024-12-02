open! Core
open! Helpers

module Snailfish = struct
  type t = Num of int | Pair of t * t [@@deriving equal, sexp]

  let rec get_pair s =
    let left, s = of_string' (String.drop_prefix s 1) in
    let right, s = of_string' (String.drop_prefix s 1) in
    (Pair (left, right), String.drop_prefix s 1)

  and get_num s =
    let n, s = Parse.take_int_exn s in
    (Num n, s)

  and of_string' s =
    match String.get s 0 with '[' -> get_pair s | _ -> get_num s

  let of_string s = fst (of_string' s)

  let to_string t =
    let rec aux acc t =
      match t with
      | Pair (t1, t2) ->
          let s = aux (acc ^ "[") t1 in
          aux (s ^ ",") t2 ^ "]"
      | Num n -> acc ^ Int.to_string n
    in
    aux "" t

  let explode_or_split t =
    let rec should_explode t depth =
      match depth with
      | 5 -> true
      | _ -> (
          match t with
          | Pair (l, r) ->
              should_explode l (depth + 1) || should_explode r (depth + 1)
          | _ -> false)
    in
    let rec should_split = function
      | Num n -> n >= 10
      | Pair (l, r) -> should_split l || should_split r
    in
    if should_explode t 0 then Some `Explode
    else if should_split t then Some `Split
    else None

  let split t =
    let rec aux t found =
      if found then (t, true)
      else
        match t with
        | Num n ->
            if n >= 10 then (Pair (Num (n / 2), Num ((n / 2) + (n % 2))), true)
            else (Num n, false)
        | Pair (l, r) ->
            let l, found = aux l false in
            if found then (Pair (l, r), true)
            else
              let r, found = aux r false in
              (Pair (l, r), found)
    in
    aux t false |> fst

  (* i probably should have just thought harder about a recursive solution but it seemed
     more obvious to just do it with strings *)
  let explode t =
    let s = to_string t in
    let rec explode_position i depth =
      if depth = 5 then i - 1
      else
        match String.get s i with
        | '[' -> explode_position (i + 1) (depth + 1)
        | ']' -> explode_position (i + 1) (depth - 1)
        | _ -> explode_position (i + 1) depth
    in
    let rec first_num_pos s i di =
      if i < 0 || i >= String.length s then None
      else if
        Char.is_digit (String.get s i)
        && not (Char.is_digit (String.get s (i - 1)))
      then Some i
      else first_num_pos s (i + di) di
    in
    let ep = explode_position 0 0 in
    let end_ep =
      String.findi s ~f:(fun i c -> i > ep && Char.equal c ']')
      |> Option.value_exn |> fst
    in
    let l, r =
      match of_string (String.slice s ep (end_ep + 1)) with
      | Pair (Num n1, Num n2) -> (n1, n2)
      | other ->
          raise_s [%message "Bad explode pair" ~snailfish_num:(other : t)]
    in
    let s = String.slice s 0 ep ^ "0" ^ String.slice s (end_ep + 1) 0 in
    let add_num s start john =
      let num, _ = Parse.take_int_exn (String.slice s start 0) in
      let l = String.length (Int.to_string num) in
      String.slice s 0 start
      ^ Int.to_string (num + john)
      ^ String.slice s (start + l) 0
    in
    let right_start = first_num_pos s (ep + 1) 1 in
    let left_start = first_num_pos s (ep - 1) (-1) in
    let s = match right_start with Some i -> add_num s i r | None -> s in
    let s = match left_start with Some i -> add_num s i l | None -> s in
    of_string s

  let reduce t =
    let rec aux t =
      match explode_or_split t with
      | None -> t
      | Some `Split -> aux (split t)
      | Some `Explode -> aux (explode t)
    in
    aux t

  let add t1 t2 = Pair (t1, t2) |> reduce

  let rec magnitude = function
    | Num n -> n
    | Pair (l, r) -> (3 * magnitude l) + (2 * magnitude r)
end

let part1 s =
  String.split_lines s
  |> List.map ~f:Snailfish.of_string
  |> List.fold ~init:None ~f:(fun acc t ->
         match acc with None -> Some t | Some l -> Some (Snailfish.add l t))
  |> Option.value_exn |> Snailfish.magnitude |> Int.to_string |> Ok

let part2 s =
  let nums = String.split_lines s |> List.map ~f:Snailfish.of_string in
  List.fold (List.cartesian_product nums nums) ~init:0 ~f:(fun acc (l, r) ->
      if Snailfish.equal l r then acc
      else
        Int.max acc (Snailfish.add l r |> Snailfish.magnitude)
        |> Int.max (Snailfish.add l r |> Snailfish.magnitude))
  |> Int.to_string |> Ok

let%expect_test "parse" =
  print_s [%sexp (Snailfish.of_string "[1,2]" : Snailfish.t)];
  [%expect {| (Pair (Num 1) (Num 2)) |}];
  print_s
    [%sexp
      (Snailfish.of_string
         "[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]"
        : Snailfish.t)];
  [%expect
    {|
       (Pair
        (Pair (Pair (Pair (Num 1) (Num 3)) (Pair (Num 5) (Num 3)))
         (Pair (Pair (Num 1) (Num 3)) (Pair (Num 8) (Num 7))))
        (Pair (Pair (Pair (Num 4) (Num 9)) (Pair (Num 6) (Num 9)))
         (Pair (Pair (Num 8) (Num 2)) (Pair (Num 7) (Num 3)))))
       |}]

let%expect_test "split" =
  let t = Snailfish.of_string "[[[[0,7],4],[15,[0,13]]],[1,1]]" in

  print_endline (Snailfish.split t |> Snailfish.to_string);
  [%expect {| [[[[0,7],4],[[7,8],[0,13]]],[1,1]] |}]

let%expect_test "explode" =
  let t = Snailfish.of_string "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]" in

  print_endline (Snailfish.explode t |> Snailfish.to_string);
  [%expect {| [[[[0,7],4],[7,[[8,4],9]]],[1,1]] |}]

let%expect_test "add" =
  let t1 = Snailfish.of_string "[[[[4,3],4],4],[7,[[8,4],9]]]" in
  let t2 = Snailfish.of_string "[1,1]" in
  print_endline (Snailfish.add t1 t2 |> Snailfish.to_string);
  [%expect {| [[[[0,7],4],[[7,8],[6,0]]],[8,1]] |}]
