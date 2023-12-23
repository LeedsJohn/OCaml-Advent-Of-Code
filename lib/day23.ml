open Core

let day = 23
let year = 2023

let parse_text text =
  String.strip text |> String.split_lines
  |> List.map ~f:String.to_array
  |> List.to_array

(* luckily the board is a square *)
let get_neighbors board x y =
  let n = Array.length board in
  let changes =
    match board.(y).(x) with
    | '^' -> [ (0, -1) ]
    | 'v' -> [ (0, 1) ]
    | '>' -> [ (1, 0) ]
    | '<' -> [ (-1, 0) ]
    | _ -> [ (1, 0); (-1, 0); (0, 1); (0, -1) ]
  in
  List.map changes ~f:(fun (dx, dy) -> (x + dx, y + dy))
  |> List.filter ~f:(fun (x, y) ->
         x >= 0 && y >= 0 && x < n && y < n && Char.(board.(y).(x) <> '#'))

let get_neighbors2 board x y =
  let n = Array.length board in
  let changes = [ (1, 0); (-1, 0); (0, 1); (0, -1) ] in
  List.map changes ~f:(fun (dx, dy) -> (x + dx, y + dy))
  |> List.filter ~f:(fun (x, y) ->
         x >= 0 && y >= 0 && x < n && y < n && Char.(board.(y).(x) <> '#'))

let pos_to_num n x y = (x * n) + y
let num_to_pos n num = (num / n, num % n)

let longest_path board (start_x, start_y) (goal_x, goal_y) =
  let n = Array.length board in
  let rec aux (x, y) visited =
    let num = pos_to_num n x y in
    if x = goal_x && y = goal_y then 0
    else if Set.mem visited num then Int.min_value
    else
      let new_visited = Set.add visited num in
      1
      + List.fold (get_neighbors board x y) ~init:Int.min_value
          ~f:(fun acc (x, y) -> Int.max acc (aux (x, y) new_visited))
  in
  aux (start_x, start_y) (Set.empty (module Int))

let part1 fname =
  let board = parse_text (In_channel.read_all fname) in
  let n = Array.length board in
  longest_path board (1, 0) (n - 2, n - 1) |> Int.to_string

let make_graph board (start_x, start_y) (goal_x, goal_y) get_neighbors =
  let n = Array.length board in
  let rec get_next_node visited (x, y) distance =
    let num = pos_to_num n x y in
    if (x = start_x && y = start_y) || (x = goal_x && y = goal_y) then
      (num, distance)
    else
      let neighbors =
        get_neighbors board x y
        |> List.filter ~f:(fun (x, y) ->
               not (Set.mem visited (pos_to_num n x y)))
      in
      if List.length neighbors <> 1 then (num, distance)
      else
        get_next_node (Set.add visited num) (List.hd_exn neighbors)
          (distance + 1)
  in
  let do_thing (x, y) =
    let starting_visited = Set.singleton (module Int) (pos_to_num n x y) in
    List.fold (get_neighbors board x y)
      ~init:(Map.empty (module Int))
      ~f:(fun acc (x, y) ->
        let next_node, distance = get_next_node starting_visited (x, y) 1 in
        Map.update acc next_node ~f:(fun d ->
            Int.max distance (Option.value d ~default:0)))
    |> Map.to_alist
  in
  let rec aux acc (x, y) =
    let num = pos_to_num n x y in
    if Map.mem acc num then acc
    else
      let neighbors = do_thing (x, y) in
      let acc = Map.add_exn acc ~key:num ~data:neighbors in
      List.fold neighbors ~init:acc ~f:(fun acc (next_node, _) ->
          aux acc (num_to_pos n next_node))
  in
  aux (Map.empty (module Int)) (start_x, start_y)

let longest_path2 graph n (start_x, start_y) (goal_x, goal_y) =
  let goal_pos = pos_to_num n goal_x goal_y in
  let rec aux visited pos =
    if pos = goal_pos then 0
    else if Set.mem visited pos then Int.min_value
    else
      let new_visited = Set.add visited pos in

      List.fold (Map.find_exn graph pos) ~init:Int.min_value
        ~f:(fun acc (next_node, distance) ->
          Int.max acc (distance + aux new_visited next_node))
  in
  aux (Set.empty (module Int)) (pos_to_num n start_x start_y)

let part2 fname =
  let board = parse_text (In_channel.read_all fname) in
  let n = Array.length board in
  let graph = make_graph board (1, 0) (n - 2, n - 1) get_neighbors2 in

  longest_path2 graph n (1, 0) (n - 2, n - 1) |> Int.to_string
