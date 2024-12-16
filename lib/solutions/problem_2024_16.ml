open! Core
open! Helpers

let of_string s = Board.of_string s

let end_pos board =
  Map.fold board ~init:(0, 0) ~f:(fun ~key ~data acc ->
      if Char.(data = 'E') then key else acc)

let start_pos board =
  Map.fold board ~init:(0, 0) ~f:(fun ~key ~data acc ->
      if Char.(data = 'S') then key else acc)

module Stuff = struct
  type t = Coordinate.t * Coordinate.t * int [@@deriving sexp]

  let compare (_, _, n1) (_, _, n2) = Int.compare n1 n2
end

module Cord2 = struct
  type t = Coordinate.t * Coordinate.t [@@deriving compare, sexp, hash]
end

module Pq = Priority_queue.Make (Stuff)

let djikstra ?(end_positions = Set.empty (module Coordinate)) board
    start_positions =
  let visited = Hashtbl.create (module Cord2) in
  let pq =
    Pq.of_list (List.map start_positions ~f:(fun (pos, dir) -> (pos, dir, 0)))
  in
  let rec aux pq =
    if Pq.is_empty pq then ()
    else
      let (pos, dir, score), pq = Pq.get_exn pq in
      if Hashtbl.mem visited (pos, dir) then aux pq
      else (
        Hashtbl.add_exn visited ~key:(pos, dir) ~data:score;
        if Set.mem end_positions pos then ()
        else
          let next_positions =
            let ldir, rdir = Coordinate.(rotate_left dir, rotate_right dir) in
            [
              (Coordinate.add pos dir, dir, score + 1);
              (pos, ldir, score + 1000);
              (pos, rdir, score + 1000);
            ]
          in
          let pq =
            List.fold next_positions ~init:pq ~f:(fun pq (pos, dir, score) ->
                match Map.find_exn board pos with
                | '#' -> pq
                | _ ->
                    if Hashtbl.mem visited (pos, dir) then pq
                    else Pq.add pq (pos, dir, score))
          in
          aux pq)
  in
  aux pq;
  visited

let part1 s =
  let board = of_string s in
  let sp, ep = (start_pos board, end_pos board) in
  let distances =
    djikstra
      ~end_positions:(Set.singleton (module Coordinate) ep)
      board
      [ (sp, (1, 0)) ]
  in
  List.find_map_exn (Coordinate.offsets |> Set.to_list) ~f:(fun dir ->
      Hashtbl.find distances (ep, dir))
  |> Int.to_string |> Ok

let part2 s =
  let board = of_string s in
  let sp, ep = (start_pos board, end_pos board) in
  let distances_from_beginning = djikstra board [ (sp, (1, 0)) ] in
  let distances_from_end =
    djikstra board
      ((List.map (Coordinate.offsets |> Set.to_list)) ~f:(fun dir -> (ep, dir)))
  in
  let best_score = Hashtbl.find_exn distances_from_end (sp, (-1, 0)) in
  Map.counti board ~f:(fun ~key:pos ~data:c ->
      if Char.(c = '#') then false
      else
        Set.exists Coordinate.offsets ~f:(fun dir ->
            let n1 =
              Hashtbl.find distances_from_beginning (pos, dir)
              |> Option.value ~default:1000000
            in
            let n2 =
              Hashtbl.find distances_from_end (pos, Coordinate.turn_around dir)
              |> Option.value ~default:1000000
            in
            n1 + n2 = best_score))
  |> Int.to_string |> Ok
