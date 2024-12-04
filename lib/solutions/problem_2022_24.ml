open! Core
open! Helpers

type tile = Wall | Blizzards of Coordinate.Direction4.t list
[@@deriving equal]

module Board = struct
  type t = { board : tile Board.t; max_x : int; max_y : int } [@@deriving equal]

  let winning_position { max_x; max_y; _ } = (max_x - 1, max_y)

  let of_string s =
    let board =
      Board.of_string s
      |> Map.map ~f:(fun c ->
             match c with
             | '>' -> Blizzards [ Coordinate.Direction4.Right ]
             | '<' -> Blizzards [ Left ]
             | '^' -> Blizzards [ Up ]
             | 'v' -> Blizzards [ Down ]
             | '.' -> Blizzards []
             | _ -> Wall)
    in
    let max_x, max_y = Board.max_coordinates board in
    { board; max_x; max_y }

  let tick_board ({ board; max_x; max_y } as t) =
    let next_blizzard_spot (x, y) (dx, dy) =
      match Map.find_exn board (x + dx, y + dy) with
      | Blizzards _ -> (x + dx, y + dy)
      | Wall -> (
          match (dx, dy) with
          | 1, 0 -> (1, y)
          | -1, 0 -> (max_x - 1, y)
          | 0, 1 -> (x, 1)
          | _ -> (x, max_y - 1))
    in
    let init =
      Map.map board ~f:(function Wall -> Wall | Blizzards _ -> Blizzards [])
    in
    let board =
      Map.fold board ~init ~f:(fun ~key:(x, y) ~data:til acc ->
          match til with
          | Wall -> acc
          | Blizzards offsets ->
              List.fold offsets ~init:acc ~f:(fun acc dir ->
                  let nx, ny =
                    next_blizzard_spot (x, y)
                      (Coordinate.Direction4.to_offset dir)
                  in
                  Map.update acc (nx, ny) ~f:(fun new_tile ->
                      let new_tile = Option.value_exn new_tile in
                      match new_tile with
                      | Wall ->
                          raise_s
                            [%message
                              "trying to move to a wall" ((nx, ny) : int * int)]
                      | Blizzards b -> Blizzards (dir :: b))))
    in
    { t with board }
end

module Board_map = struct
  type t = { boards : Board.t Map.M(Int).t; num_boards : int }

  let of_board initial_board =
    let rec aux board step acc =
      let next_board = Board.tick_board board in
      if Board.equal next_board initial_board then
        { boards = acc; num_boards = step }
      else
        aux next_board (step + 1) (Map.add_exn acc ~key:step ~data:next_board)
    in
    aux initial_board 1 (Map.singleton (module Int) 0 initial_board)

  let get { boards; num_boards } step = Map.find_exn boards (step % num_boards)
end

module Game_position = struct
  module T = struct
    type t = { player : Coordinate.t; step : int }
    [@@deriving compare, sexp_of, hash]
  end

  include T
  include Comparator.Make (T)

  let neighbors { player; step } (boards : Board_map.t) =
    let next_step = (step + 1) % boards.num_boards in
    let next_board = Board_map.get boards next_step in
    List.map
      (player :: (Coordinate.neighbors player |> Set.to_list))
      ~f:(fun player -> { player; step = next_step })
    |> List.filter ~f:(fun { player; _ } ->
           match Map.find next_board.board player with
           | Some (Blizzards []) -> true
           | _ -> false)
    |> List.filter ~f:(fun { player = x, y; _ } ->
           Int.between x ~low:0 ~high:next_board.max_x
           && Int.between y ~low:0 ~high:next_board.max_y)
end

let bfs ~board ~start_pos ~end_pos ~start_step =
  let board_map = Board_map.of_board board in
  let (start_position : Game_position.t) =
    { player = start_pos; step = start_step }
  in
  let rec aux step (game_positions : Set.M(Game_position).t) =
    if
      Set.exists game_positions ~f:(fun { player; _ } ->
          Coordinate.equal player end_pos)
    then step
    else
      let next_positions =
        List.map (Set.to_list game_positions) ~f:(fun pos ->
            Game_position.neighbors pos board_map)
        |> List.join
        |> Set.of_list (module Game_position)
      in
      aux (step + 1) next_positions
  in
  aux 0 (Set.singleton (module Game_position) start_position)

let part1 s =
  let board = Board.of_string s in
  let end_pos = Board.winning_position board in
  Ok (bfs ~board ~start_pos:(1, 0) ~end_pos ~start_step:0 |> Int.to_string)

let part2 s =
  let board = Board.of_string s in
  let start_pos = (1, 0) in
  let end_pos = Board.winning_position board in
  let step1 = bfs ~board ~start_pos ~end_pos ~start_step:0 in
  let step2 =
    bfs ~board ~start_pos:end_pos ~end_pos:start_pos ~start_step:step1
  in
  let step3 = bfs ~board ~start_pos ~end_pos ~start_step:(step1 + step2) in
  Ok (step1 + step2 + step3 |> Int.to_string)
