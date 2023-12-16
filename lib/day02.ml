open Core

let day = 2
let year = 2023

type round = { red : int; green : int; blue : int } [@@deriving sexp]

let parse_line line =
  let line =
    String.split_on_chars line ~on:[ ';'; ':' ]
    |> List.tl_exn
    |> List.map ~f:(String.split ~on:' ')
  in
  List.map line ~f:(fun cubes ->
      let cubes = Array.of_list cubes in
      let r = ref { red = 0; green = 0; blue = 0 } in
      let i = ref 1 in
      while !i < Array.length cubes do
        let n = Int.of_string cubes.(!i) in
        (match String.get cubes.(!i + 1) 0 with
        | 'r' -> r := { !r with red = n }
        | 'g' -> r := { !r with green = n }
        | _ -> r := { !r with blue = n });
        i := !i + 2
      done;
      !r)

let parse_input lines = List.map lines ~f:parse_line

let part1 fname =
  let lines = parse_input (In_channel.read_lines fname) in
  let valid_line rounds =
    List.for_all rounds ~f:(fun r ->
        r.red <= 12 && r.green <= 13 && r.blue <= 14)
  in
  List.foldi lines ~init:0 ~f:(fun i acc rounds ->
      if valid_line rounds then i + acc + 1 else acc)
  |> Int.to_string

let part2 fname =
  let lines = parse_input (In_channel.read_lines fname) in
  let line_power rounds =
    let r, g, b =
      List.fold rounds ~init:(0, 0, 0) ~f:(fun (r, g, b) round ->
          (Int.max r round.red, Int.max g round.green, Int.max b round.blue))
    in
    r * g * b
  in
  List.fold lines ~init:0 ~f:(fun acc l -> acc + line_power l) |> Int.to_string

let%expect_test "parsing" =
  let lines =
    {|Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green|}
    |> String.split_lines
  in
  let lines = parse_input lines in
  print_s [%sexp (lines : round list list)];
  [%expect
    {|
    ((((red 4) (green 0) (blue 3)) ((red 1) (green 2) (blue 6))
      ((red 0) (green 2) (blue 0)))
     (((red 0) (green 2) (blue 1)) ((red 1) (green 3) (blue 4))
      ((red 0) (green 1) (blue 1)))
     (((red 20) (green 8) (blue 6)) ((red 4) (green 13) (blue 5))
      ((red 1) (green 5) (blue 0)))
     (((red 3) (green 1) (blue 6)) ((red 6) (green 3) (blue 0))
      ((red 14) (green 3) (blue 15)))
     (((red 6) (green 3) (blue 1)) ((red 1) (green 2) (blue 2)))) |}];
  ()
