open Core

let day = 14
let year = 2023

let parse_text text =
  String.strip text |> String.split_lines
  |> List.map ~f:String.to_array
  |> Array.of_list

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

(* luckily the input is a square *)
let rotate_piece board square_depth i =
  let n = Array.length board in
  let _ =
    List.fold
      [
        (square_depth + i, square_depth);
        (n - 1 - square_depth, square_depth + i);
        (n - 1 - square_depth - i, n - 1 - square_depth);
        (square_depth, n - 1 - square_depth - i);
      ]
      ~init:board.(n - 1 - square_depth - i).(square_depth)
      ~f:(fun acc (x, y) ->
        let temp = board.(y).(x) in
        board.(y).(x) <- acc;
        temp)
  in
  ()

let rotate_cw board =
  let n = Array.length board in
  for square_depth = 0 to (n / 2) - 1 do
    for i = 0 to n - (square_depth * 2) - 2 do
      rotate_piece board square_depth i
    done
  done

let complete_cycle board =
  for _ = 0 to 3 do
    tilt_board_north board;
    rotate_cw board
  done

let board_to_string board =
  Array.fold board ~init:"" ~f:(fun acc row -> acc ^ String.of_array row)

let part2 fname =
  let board = parse_text (In_channel.read_all fname) in
  let visited = Hashtbl.create (module String) in
  let goal_cycles = 1000000000 in
  Hashtbl.add_exn visited ~key:(board_to_string board) ~data:0;
  let cycle_length = ref None in
  let i = ref 0 in
  while Option.is_none !cycle_length do
    i := !i + 1;
    complete_cycle board;
    let s = board_to_string board in
    match Hashtbl.find visited s with
    | None -> Hashtbl.add_exn visited ~key:s ~data:!i
    | Some n -> cycle_length := Some (!i - n)
  done;
  let num_remaining = (goal_cycles - !i) % Option.value_exn !cycle_length in
  for _ = 0 to num_remaining - 1 do
    complete_cycle board
  done;
  score_board board |> Int.to_string

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

let%expect_test "rotate" =
  let board = parse_text {|##....
##....
.#....
.#....
.#...#
.#..##|} in
  rotate_cw board;
  print_board board;
  [%expect
    {|
    ....##
    ######
    ......
    ......
    #.....
    ##.... |}];
  ()

let%expect_test "cycle" =
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
  complete_cycle board;
  print_board board;
  [%expect
    {|
    .....#....
    ....#...O#
    ...OO##...
    .OO#......
    .....OOO#.
    .O#...O#.#
    ....O#....
    ......OOOO
    #...O###..
    #..OO#.... |}]
