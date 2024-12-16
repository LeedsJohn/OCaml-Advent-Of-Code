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

let solve board start_pos end_pos =
  let visited = Hash_set.create (module Cord2) in
  Hash_set.add visited (start_pos, (1, 0));
  let pq = Pq.singleton (start_pos, (1, 0), 0) in
  let rec djikstra pq =
    let (pos, dir, score), pq = Pq.get_exn pq in
    Hash_set.add visited (pos, dir);
    if Coordinate.equal pos end_pos then score
    else
      let next_positions =
        [
          (Coordinate.add pos dir, dir, score + 1);
          (pos, Coordinate.rotate_left dir, score + 1000);
          (pos, Coordinate.rotate_right dir, score + 1000);
        ]
      in
      let pq =
        List.fold next_positions ~init:pq ~f:(fun pq (pos, dir, score) ->
            match Map.find_exn board pos with
            | '#' -> pq
            | _ ->
                if Hash_set.mem visited (pos, dir) then pq
                else Pq.add pq (pos, dir, score))
      in
      djikstra pq
  in
  djikstra pq

let solve2 board start_pos end_pos =
  let rec djikstra pq memo =
    if Pq.is_empty pq then ()
    else
      let (pos, dir, score), pq = Pq.get_exn pq in
      if Hashtbl.mem memo (pos, dir) then djikstra pq memo
      else (
        Hashtbl.add_exn memo ~key:(pos, dir) ~data:score;
        let next_positions =
          [
            (Coordinate.add pos dir, dir, score + 1);
            (pos, Coordinate.rotate_left dir, score + 1000);
            (pos, Coordinate.rotate_right dir, score + 1000);
          ]
        in
        let pq =
          List.fold next_positions ~init:pq ~f:(fun pq (pos, dir, score) ->
              match Map.find_exn board pos with
              | '#' -> pq
              | _ ->
                  if Hashtbl.mem memo (pos, dir) then pq
                  else Pq.add pq (pos, dir, score))
        in
        djikstra pq memo)
  in
  let best_score = solve board start_pos end_pos in
  let distance_from_start = Hashtbl.create (module Cord2) in
  let distance_from_end = Hashtbl.create (module Cord2) in
  djikstra (Pq.singleton (start_pos, (1, 0), 0)) distance_from_start;
  djikstra
    (Pq.of_list
       [
         (end_pos, (1, 0), 0);
         (end_pos, (-1, 0), 0);
         (end_pos, (0, 1), 0);
         (end_pos, (0, -1), 0);
       ])
    distance_from_end;
  Hashtbl.fold
    ~init:(Set.empty (module Coordinate))
    distance_from_start
    ~f:(fun ~key:(end_pos, end_dir) ~data:dist acc ->
      let score =
        Hashtbl.find distance_from_end (end_pos, Coordinate.scale end_dir (-1))
        |> Option.value ~default:1000000000000
      in
      if score + dist = best_score then Set.add acc end_pos else acc)
  |> Set.length

let part1 s =
  let board = of_string s in
  solve board (start_pos board) (end_pos board) |> Int.to_string |> Ok

let part2 s =
  let board = of_string s in
  solve2 board (start_pos board) (end_pos board) |> Int.to_string |> Ok
