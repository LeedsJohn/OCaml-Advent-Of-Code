open! Core

let day = 9
let year = 2023

let parse_text text =
  String.split_lines text
  |> List.map ~f:(fun l -> List.map (String.split l ~on:' ') ~f:Int.of_string)

let extend line =
  let rec aux lines =
    if List.for_all (List.hd_exn lines) ~f:(Int.equal 0) then List.rev lines
    else
      let new_line =
        List.fold (List.hd_exn lines) ~init:([], None)
          ~f:(fun (l, prev_num) n ->
            match prev_num with
            | None -> (l, Some n)
            | Some prev_num -> ((n - prev_num) :: l, Some n))
        |> fst |> List.rev
      in
      aux (new_line :: lines)
  in
  aux [ line ]

let get_prediction line =
  let lines = extend line |> List.map ~f:List.rev |> List.rev in
  List.fold lines ~init:0 ~f:(fun prev_num l -> List.hd_exn l + prev_num)

let part1 fname =
  let lines = parse_text (In_channel.read_all fname) in
  List.fold lines ~init:0 ~f:(fun acc line -> acc + get_prediction line)
  |> Int.to_string

let get_front_prediction line =
  let lines = extend line |> List.rev in
  List.fold lines ~init:0 ~f:(fun prev_num l -> List.hd_exn l - prev_num)

let part2 fname =
  let lines = parse_text (In_channel.read_all fname) in
  List.fold lines ~init:0 ~f:(fun acc line -> acc + get_front_prediction line)
  |> Int.to_string

let%expect_test "extend lines" =
  let text = {|0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45|} in
  let input = parse_text text in
  print_s [%sexp (extend (List.hd_exn input) : int list list)];
  [%expect {| ((0 3 6 9 12 15) (3 3 3 3 3) (0 0 0 0)) |}];
  print_s [%sexp (get_prediction (List.hd_exn input) : int)];
  [%expect {| 18 |}];
  ()
