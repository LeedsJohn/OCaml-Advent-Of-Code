open Core

let day = 1
let year = 2023
let get_input fname = In_channel.read_lines fname

let calibration_digit line =
  let num1 =
    String.find line ~f:Char.is_digit |> Option.value_exn |> Char.get_digit_exn
  in
  let num2 =
    String.find (String.rev line) ~f:Char.is_digit
    |> Option.value_exn |> Char.get_digit_exn
  in
  (num1 * 10) + num2

let numbers =
  let keys =
    List.map (List.range 1 10) ~f:Int.to_string
    @ [ "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" ]
  in
  let values = List.range 1 10 @ List.range 1 10 in
  List.zip_exn keys values

let get_first_num line =
  String.find_mapi line ~f:(fun i _ ->
      List.find_map numbers ~f:(fun (k, v) ->
          if i + String.length k > String.length line then None
          else if String.equal k (String.slice line i (i + String.length k))
          then Some v
          else None))
  |> Option.value_exn

let get_last_num line =
  List.find_map
    (List.range ~stride:(-1) (String.length line) (-1))
    ~f:(fun i ->
      List.find_map numbers ~f:(fun (k, v) ->
          if i + String.length k > String.length line then None
          else if String.equal k (String.slice line i (i + String.length k))
          then Some v
          else None))
  |> Option.value_exn

let part1 fname =
  get_input fname
  |> List.map ~f:calibration_digit
  |> List.fold ~init:0 ~f:( + ) |> Int.to_string

let part2 fname =
  get_input fname
  |> List.map ~f:(fun l -> (get_first_num l * 10) + get_last_num l)
  |> List.fold ~init:0 ~f:( + ) |> Int.to_string

let%expect_test "stuff" =
  let stuff =
    {|eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen|}
    |> String.split_lines
    |> List.map ~f:(fun l -> (get_first_num l, get_last_num l))
  in
  print_s [%sexp (stuff : (int * int) list)];
  [%expect {| ((8 3) (1 3) (2 4) (4 2) (1 4) (7 6)) |}]
