open Core

let day = 14
let year = 2023

let parse_text text =
  String.split_lines text |> List.map ~f:String.to_array |> Array.of_list

let tilt_col_north board x =
  let open_y = ref 0 in
  let get_next_open y =
    let y = ref y in
    while !y < Array.length board && Char.(board.(!y).(x) <> '.') do
      y := !y + 1
    done;
    !y
  in
  open_y := get_next_open !open_y;
  for y = 0 to Array.length board - 1 do
    match board.(y).(x) with
    | '.' -> ()
    | '#' -> open_y := get_next_open (y + 1)
    | _ ->
        if y > !open_y then (
          board.(!open_y).(x) <- 'O';
          board.(y).(x) <- '.';
          open_y := get_next_open (!open_y + 1))
  done

let tilt_board_north board =
  Array.iteri board.(0) ~f:(fun x _ -> tilt_col_north board x)

let score_board board =
  let num_rows = Array.length board in
  let score_col x =
    Array.foldi board ~init:0 ~f:(fun y acc _ ->
        if Char.(board.(y).(x) <> 'O') then acc else acc + (num_rows - y))
  in
  Array.foldi board.(0) ~init:0 ~f:(fun x acc _ -> acc + score_col x)

let part1 fname =
  let board = parse_text (In_channel.read_all fname) in
  tilt_board_north board;
  score_board board |> Int.to_string

let part2 fname = fname

let print_board board =
  Array.iter board ~f:(fun row ->
      Array.iter row ~f:(fun c -> printf "%c" c);
      print_endline "")

let%expect_test "tilt" =
  let board =
    parse_text
      {|OOOO.#.O..
OO..#....#
OO..O##..O
O..#.OO...
........#.
..#....#.#
..O..#.O.O
..O.......
#....###..
#....#....|}
  in
  tilt_board_north board;
  print_board board;
  [%expect
    {|
    OOOO.#.O..
    OO..#....#
    OO..O##..O
    O..#.OO...
    ........#.
    ..#....#.#
    ..O..#.O.O
    ..O.......
    #....###..
    #....#.... |}]
