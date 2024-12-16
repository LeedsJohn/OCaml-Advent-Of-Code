open! Core
open! Helpers

type tile = Wall | Empty | Box

module Warehouse = struct
  type t = { board : tile Board.t; pos : Coordinate.t }

  let of_string s =
    let board = Board.of_string s in
    let pos =
      Map.fold board ~init:(0, 0) ~f:(fun ~key:pos ~data:c acc ->
          if Char.equal c '@' then pos else acc)
    in
    let board =
      Map.map board ~f:(function '#' -> Wall | 'O' -> Box | _ -> Empty)
    in
    { board; pos }

  [@@@warning "-32"]

  let to_string { board; pos } =
    Map.mapi board ~f:(fun ~key ~data ->
        if Coordinate.equal key pos then '@'
        else match data with Wall -> '#' | Empty -> '.' | Box -> 'O')
    |> Board.to_string

  let shuffle_boxes ({ board; pos } as t) dir =
    let rec get_end_point board pos =
      match Map.find_exn board pos with
      | Wall -> None
      | Empty -> Some pos
      | Box -> get_end_point board (Coordinate.add pos dir)
    in
    match get_end_point board (Coordinate.add pos dir) with
    | None -> t
    | Some end_point ->
        let board =
          Map.set board ~key:(Coordinate.add pos dir) ~data:Empty
          |> Map.set ~key:end_point ~data:Box
        in
        { board; pos = Coordinate.add pos dir }

  let apply_move ({ board; pos } as t) dir =
    match Map.find_exn board (Coordinate.add pos dir) with
    | Empty -> { board; pos = Coordinate.add pos dir }
    | Wall -> t
    | Box -> shuffle_boxes t dir

  let apply_moves t dir_list = List.fold dir_list ~init:t ~f:apply_move

  let gps_coords { board; _ } =
    Map.sumi
      (module Int)
      board
      ~f:(fun ~key:(x, y) ~data:til ->
        match til with Box -> (100 * y) + x | _ -> 0)
end

module Warehouse2 = struct
  type tile = Wall | Empty | Box

  type t = {
    walls : Set.M(Coordinate).t;
    boxes : Coordinate.t Map.M(Coordinate).t;
    pos : Coordinate.t;
  }

  let get_spot { walls; boxes; _ } pos =
    if Set.mem walls pos then Wall else if Map.mem boxes pos then Box else Empty

  let of_string s =
    let s =
      String.split_lines s |> List.map ~f:String.to_list
      |> List.map ~f:(fun line ->
             List.map line ~f:(function
               | '#' -> [ '#'; '#' ]
               | 'O' -> [ '['; ']' ]
               | '.' -> [ '.'; '.' ]
               | '@' -> [ '@'; '.' ]
               | _ -> raise_s [%message "bad character in board"])
             |> List.join)
    in
    let walls, boxes, pos =
      List.foldi s
        ~init:
          (Set.empty (module Coordinate), Map.empty (module Coordinate), (0, 0))
        ~f:(fun y acc line ->
          List.foldi line ~init:acc ~f:(fun x (walls, boxes, pos) c ->
              match c with
              | '#' -> (Set.add walls (x, y), boxes, pos)
              | '@' -> (walls, boxes, (x, y))
              | '[' ->
                  let l = (x, y) in
                  let r = (x + 1, y) in
                  ( walls,
                    Map.add_exn boxes ~key:l ~data:r
                    |> Map.add_exn ~key:r ~data:l,
                    pos )
              | '.' | ']' -> (walls, boxes, pos)
              | c ->
                  raise_s
                    [%message "invalid character for warehouse" (c : char)]))
    in
    { walls; boxes; pos }

  let _to_string { boxes; walls; pos } =
    let board =
      Set.fold walls
        ~init:(Map.singleton (module Coordinate) pos '@')
        ~f:(fun (acc : char Map.M(Coordinate).t) (pos : Coordinate.t) ->
          Map.add_exn acc ~key:pos ~data:'#')
    in
    Map.fold boxes ~init:board ~f:(fun ~key:pos ~data:neighbor acc ->
        if Coordinate.(equal (add pos (1, 0)) neighbor) then
          Map.add_exn acc ~key:pos ~data:'['
          |> Map.add_exn ~key:neighbor ~data:']'
        else acc)
    |> Board.to_string

  let shuffle_boxes ({ walls; boxes; pos } as t) dir =
    let rec get_shuffled_boxes cur_level prev_boxes =
      let next_level =
        Set.map
          (module Coordinate)
          cur_level
          ~f:(fun pos -> Coordinate.add pos dir)
        |> Set.fold
             ~init:(Set.empty (module Coordinate))
             ~f:(fun acc pos ->
               match Map.find boxes pos with
               | Some other ->
                   Set.union acc
                     (Set.of_list (module Coordinate) [ pos; other ])
               | None -> Set.add acc pos)
        |> Set.filter ~f:(fun pos -> not (Set.mem prev_boxes pos))
        |> Set.filter ~f:(fun pos ->
               match get_spot t pos with Empty -> false | _ -> true)
        |> Set.fold
             ~init:(Some (Set.empty (module Coordinate)))
             ~f:(fun acc pos ->
               match (acc, get_spot t pos) with
               | None, _ -> None
               | _, Wall -> None
               | Some acc, Box -> Some (Set.add acc pos)
               | _, _ -> None)
      in
      match next_level with
      | None -> None
      | Some next_level ->
          if Set.length next_level = 0 then Some prev_boxes
          else get_shuffled_boxes next_level (Set.union next_level prev_boxes)
    in
    let initial_boxes =
      Set.of_list
        (module Coordinate)
        [ Coordinate.add pos dir; Map.find_exn boxes (Coordinate.add pos dir) ]
    in
    match get_shuffled_boxes initial_boxes initial_boxes with
    | None -> t
    | Some shuffled_boxes ->
        let boxes =
          Map.mapi boxes ~f:(fun ~key:pos ~data:neighbor ->
              if Set.mem shuffled_boxes pos then Coordinate.add neighbor dir
              else neighbor)
          |> Map.map_keys_exn
               (module Coordinate)
               ~f:(fun pos ->
                 if Set.mem shuffled_boxes pos then Coordinate.add pos dir
                 else pos)
        in
        { walls; boxes; pos = Coordinate.add pos dir }

  let apply_move ({ pos; _ } as t) dir =
    match get_spot t (Coordinate.add pos dir) with
    | Empty -> { t with pos = Coordinate.add pos dir }
    | Wall -> t
    | Box -> shuffle_boxes t dir

  let apply_moves t dir_list = List.fold dir_list ~init:t ~f:apply_move

  let gps_coords { boxes; _ } =
    Map.sumi
      (module Int)
      boxes
      ~f:(fun ~key:(x1, y1) ~data:(x2, y2) ->
        let x, y = if x1 < x2 then (x1, y1) else (x2, y2) in
        (100 * y) + x)
    / 2
end

let get_dir_list s =
  String.filter s ~f:(fun c -> not (Char.is_whitespace c))
  |> String.to_list
  |> List.map ~f:(function
       | 'v' -> (0, 1)
       | '^' -> (0, -1)
       | '>' -> (1, 0)
       | '<' -> (-1, 0)
       | _ -> raise_s [%message "bad move character"])

let get_warehouse_and_dir_list s of_string =
  let board_string, dir_string =
    match
      String.substr_replace_first s ~pattern:"\n\n" ~with_:"*"
      |> String.split ~on:'*'
    with
    | [ board_string; dir_string ] -> (board_string, dir_string)
    | _ -> raise_s [%message "malformed string"]
  in
  (of_string board_string, get_dir_list dir_string)

let part1 s =
  let warehouse, dir_list = get_warehouse_and_dir_list s Warehouse.of_string in
  Warehouse.apply_moves warehouse dir_list
  |> Warehouse.gps_coords |> Int.to_string |> Ok

let part2 s =
  let warehouse, dir_list = get_warehouse_and_dir_list s Warehouse2.of_string in
  Warehouse2.apply_moves warehouse dir_list
  |> Warehouse2.gps_coords |> Int.to_string |> Ok
