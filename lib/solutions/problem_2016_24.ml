open! Core
open Helpers

module Position : sig
  type t = private int [@@deriving compare, sexp]

  include Comparator.S with type t := t

  val get : unit -> t
end = struct
  module T = struct
    type t = int [@@deriving compare, sexp]
  end

  include Comparator.Make (T)
  include T

  let counter = ref (-1)

  let get () =
    incr counter;
    !counter
  ;;
end

module Board : sig
  type t

  val parse : string -> t
  val neighbors : t -> Position.t -> int Map.M(Position).t
  val debug_print : t -> unit
end = struct
  type t =
    { neighbors : int Map.M(Position).t Map.M(Position).t
    ; goal_positions : Position.t Map.M(Char).t
    ; position_to_coord : Coordinate.t Map.M(Position).t
    }

  (* CR: make sure every goal position is in the edge map *)
  let parse s =
    let ar = String.split_lines s |> Array.of_list_map ~f:String.to_array in
    let get_neighbors pos =
      let neighbors coord =
        Coordinate.neighbors coord
        |> List.filter ~f:(fun neighbor ->
          let x, y = Coordinate.xy neighbor in
          not ([%equal: Char.t] '#' ar.(y).(x)))
      in
      let num_options coord = neighbors coord |> List.length in
      let rec aux ~pos ~prev ~dist =
        match num_options pos > 2 with
        | true -> Some (pos, dist)
        | false ->
          let next_positions =
            neighbors pos
            |> List.filter ~f:(fun next_pos -> not ([%equal: Coordinate.t] next_pos prev))
          in
          (match next_positions with
           | [] -> None
           | [ next_pos ] -> aux ~pos:next_pos ~prev:pos ~dist:(dist + 1)
           | _ -> failwith "impossible")
      in
      neighbors pos
      |> List.filter_map ~f:(fun first_pos -> aux ~pos:first_pos ~prev:pos ~dist:1)
    in
    let get_edge_map start_pos =
      let rec aux pos res =
        print_s [%message "finding neighbors" (pos : Coordinate.t)];
        let neighbors = get_neighbors pos in
        let res =
          Map.add_exn res ~key:pos ~data:(Map.of_alist_exn (module Coordinate) neighbors)
        in
        List.unzip neighbors
        |> fst
        |> List.fold ~init:res ~f:(fun acc neighbor ->
          match Map.mem acc neighbor with
          | true -> acc
          | false -> aux neighbor acc)
      in
      aux start_pos (Map.empty (module Coordinate))
    in
    let goal_positions =
      List.cartesian_product
        (List.range 0 (Array.length ar.(0)))
        (List.range 0 (Array.length ar))
      |> List.fold
           ~init:(Map.empty (module Char))
           ~f:(fun acc (x, y) ->
             match ar.(y).(x) with
             | '.' | '#' -> acc
             | c -> Map.add_exn acc ~key:c ~data:(Coordinate.make ~x ~y))
    in
    let coord_edge_map = get_edge_map (Map.find_exn goal_positions '0') in
    let position_to_coord =
      Map.keys coord_edge_map
      |> List.map ~f:(fun coord -> Position.get (), coord)
      |> Map.of_alist_exn (module Position)
    in
    let coord_to_position =
      Map.to_alist position_to_coord
      |> List.map ~f:(fun (pos, coord) -> coord, pos)
      |> Map.of_alist_exn (module Coordinate)
    in
    let neighbors =
      Map.map_keys_exn
        (module Position)
        coord_edge_map
        ~f:(Map.find_exn coord_to_position)
      |> Map.map
           ~f:(Map.map_keys_exn (module Position) ~f:(Map.find_exn coord_to_position))
    in
    let goal_positions = Map.map goal_positions ~f:(Map.find_exn coord_to_position) in
    { neighbors; goal_positions; position_to_coord }
  ;;

  let neighbors { neighbors; goal_positions = _; position_to_coord = _ } pos =
    Map.find_exn neighbors pos
  ;;

  let debug_print { neighbors; goal_positions; position_to_coord } =
    let f = Map.find_exn position_to_coord in
    let goal_positions = Map.map goal_positions ~f in
    let neighbors =
      Map.map neighbors ~f:(Map.map_keys_exn (module Coordinate) ~f)
      |> Map.map_keys_exn (module Coordinate) ~f
    in
    print_s
      [%message
        (neighbors : int Map.M(Coordinate).t Map.M(Coordinate).t)
          (goal_positions : Coordinate.t Map.M(Char).t)]
  ;;
end

let part1 s =
  let board = Board.parse s in
  Board.debug_print board;
  ""
;;

let part2 _ = raise_s [%message "unimplemented"]

let%expect_test "parsing" =
  let board =
    Board.parse
      {|###########
#0.1.....2#
#.#######.#
#4.......3#
###########|}
  in
  Board.debug_print board;
  [%expect {| |}]
;;
