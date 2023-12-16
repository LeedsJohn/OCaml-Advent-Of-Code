open Core

let day = 13
let year = 2023

let parse_text text =
  let text = String.substr_replace_all text ~pattern:"\n\n" ~with_:"Z" in
  let boards = String.split text ~on:'Z' in
  List.map boards ~f:(fun board ->
      List.map (String.split_lines board) ~f:String.to_array |> Array.of_list)

let check_vertical_differences board left_x =
  let res = ref 0 in
  List.iter
    (List.range 0 (Int.min (left_x + 1) (Array.length board.(0) - left_x - 1)))
    ~f:(fun dx ->
      Array.iteri board ~f:(fun y _ ->
          if Char.(board.(y).(left_x - dx) <> board.(y).(left_x + 1 + dx)) then
            res := !res + 1));
  !res

let check_horizontal_differences board up_y =
  let res = ref 0 in
  List.iter
    (List.range 0 (Int.min (up_y + 1) (Array.length board - up_y - 1)))
    ~f:(fun dy ->
      Array.iteri board.(0) ~f:(fun x _ ->
          if Char.(board.(up_y - dy).(x) <> board.(up_y + 1 + dy).(x)) then
            res := !res + 1));
  !res

let check_vertical_mirror board left_x =
  if left_x + 1 = Array.length board.(0) then false
  else check_vertical_differences board left_x = 0

let check_horizontal_mirror board up_y =
  if up_y + 1 = Array.length board then false
  else check_horizontal_differences board up_y = 0

let get_horizontal_mirror_pos board =
  Array.findi board ~f:(fun i _ -> check_horizontal_mirror board i)
  |> Option.value_map ~default:None ~f:(fun (i, _) -> Some i)

let get_horizontal_mirror board =
  match get_horizontal_mirror_pos board with
  | None -> None
  | Some n -> Some (n + 1)

let get_vertical_mirror_pos board =
  Array.findi board.(0) ~f:(fun i _ -> check_vertical_mirror board i)
  |> Option.value_map ~default:None ~f:(fun (i, _) -> Some i)

let get_vertical_mirror board =
  match get_vertical_mirror_pos board with
  | None -> None
  | Some n -> Some (n + 1)

let get_board_score board =
  match (get_vertical_mirror board, get_horizontal_mirror board) with
  | Some n, _ -> Some n
  | _, Some n -> Some (100 * n)
  | _, _ -> None

let part1 fname =
  let boards = parse_text (In_channel.read_all fname) in
  List.fold boards ~init:0 ~f:(fun acc board ->
      acc + Option.value_exn (get_board_score board))
  |> Int.to_string

let check_vertical_mirror' board left_x =
  if left_x + 1 = Array.length board.(0) then false
  else check_vertical_differences board left_x = 1

let check_horizontal_mirror' board up_y =
  if up_y + 1 = Array.length board then false
  else check_horizontal_differences board up_y = 1

let get_horizontal_mirror_pos' board =
  Array.findi board ~f:(fun i _ -> check_horizontal_mirror' board i)
  |> Option.value_map ~default:None ~f:(fun (i, _) -> Some i)

let get_horizontal_mirror' board =
  match get_horizontal_mirror_pos' board with
  | None -> None
  | Some n -> Some (n + 1)

let get_vertical_mirror_pos' board =
  Array.findi board.(0) ~f:(fun i _ -> check_vertical_mirror' board i)
  |> Option.value_map ~default:None ~f:(fun (i, _) -> Some i)

let get_vertical_mirror' board =
  match get_vertical_mirror_pos' board with
  | None -> None
  | Some n -> Some (n + 1)

let get_board_score' board =
  match (get_vertical_mirror' board, get_horizontal_mirror' board) with
  | Some n, _ -> Some n
  | _, Some n -> Some (100 * n)
  | _, _ -> None

let part2 fname =
  let boards = parse_text (In_channel.read_all fname) in
  List.fold boards ~init:0 ~f:(fun acc board ->
      acc + Option.value_exn (get_board_score' board))
  |> Int.to_string
