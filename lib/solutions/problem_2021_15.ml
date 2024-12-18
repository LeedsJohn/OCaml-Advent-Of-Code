open! Core
open! Helpers

module Pq_elem = struct
  module T = struct
    type t = { weight : int; coord : Coordinate.t } [@@deriving sexp_of]

    let compare { weight = w1; _ } { weight = w2; _ } = Int.compare w1 w2
  end

  include T
  include Comparator.Make (T)
end

module Pq = Priority_queue.Make (Pq_elem)

module Funny_board = struct
  type t = { board : int Board.t; dim : int; goal_pos : Coordinate.t }

  let wrap_val dist n = if n + dist < 10 then n + dist else (n + dist + 1) % 10

  let of_string s copies =
    let board = Board.of_string s |> Map.map ~f:Char.get_digit_exn in
    let dim = (Board.max_coordinates board |> fst) + 1 in
    { board; dim; goal_pos = ((dim * copies) - 1, (dim * copies) - 1) }

  let find { board; dim; goal_pos = x2, y2 } (x, y) =
    if Int.min x y < 0 || x > x2 || y > y2 then None
    else
      let dist = (x / dim) + (y / dim) in
      Some (wrap_val dist (Map.find_exn board (x % dim, y % dim)))
end

let dijkstra (board : Funny_board.t) =
  let visited = Hash_set.create (module Coordinate) in
  let rec aux pq =
    let ({ weight; coord } : Pq_elem.t), pq = Pq.get_exn pq in
    if Hash_set.mem visited coord then aux pq
    else (
      Hash_set.add visited coord;
      if Coordinate.equal coord board.goal_pos then weight
      else
        let pq =
          Set.fold
            (Coordinate.neighbors coord |> Set.of_list (module Coordinate))
            ~init:pq
            ~f:(fun acc coord ->
              match Funny_board.find board coord with
              | None -> acc
              | Some next ->
                  if Hash_set.mem visited coord then acc
                  else Pq.add acc { weight = weight + next; coord })
        in
        aux pq)
  in
  aux (Pq.singleton { weight = 0; coord = (0, 0) })

let part1 s =
  let board = Funny_board.of_string s 1 in
  Ok (Int.to_string (dijkstra board))

let part2 s =
  let board = Funny_board.of_string s 5 in
  Ok (Int.to_string (dijkstra board))
