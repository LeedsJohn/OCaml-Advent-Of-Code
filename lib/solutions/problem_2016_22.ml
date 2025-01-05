open! Core
open! Helpers

module Nodes = struct
  type t = {
    board : (int * int) Map.M(Coordinate).t;
    goal_data_pos : Coordinate.t;
    empty_space : int * int;
  }
  [@@deriving sexp_of, compare, hash]

  let of_string s : t =
    let lines = String.split_lines s in
    let board =
      List.drop lines 2
      |> List.fold
           ~init:(Map.empty (module Coordinate))
           ~f:(fun acc line ->
             let pos, used, avail =
               match Parse.line_numbers line with
               | [ x; y; _size; used; avail; _percent ] -> ((x, y), used, avail)
               | other -> raise_s [%message "bad line" ~line:(other : int list)]
             in
             Map.add_exn acc ~key:pos ~data:(used, avail))
    in
    let goal_data_pos =
      Map.keys board
      |> List.fold ~init:(0, 0) ~f:(fun ((big_x, _) as acc) (x, y) ->
             if y = 0 && x > big_x then (x, y) else acc)
    in
    let empty_space =
      Map.filter board ~f:(fun (used, _) -> used = 0) |> Map.keys |> List.hd_exn
    in
    { board; goal_data_pos; empty_space }

  let viable_pair { board; _ } a_pos b_pos =
    if Coordinate.equal a_pos b_pos then false
    else
      match (Map.find board a_pos, Map.find board b_pos) with
      | None, _ | _, None -> false
      | Some (a_used, _), Some (_, b_avail) -> a_used > 0 && a_used <= b_avail

  let move_data { board; goal_data_pos; empty_space = _ } start_pos end_pos =
    let (a_used, a_avail), (b_used, b_avail) =
      (Map.find_exn board start_pos, Map.find_exn board end_pos)
    in
    let board = Map.set board ~key:start_pos ~data:(0, a_avail + a_used) in
    let board =
      Map.set board ~key:end_pos ~data:(b_used + a_used, b_avail - a_used)
    in
    if Coordinate.equal goal_data_pos start_pos then
      { board; goal_data_pos = end_pos; empty_space = start_pos }
    else { board; goal_data_pos; empty_space = start_pos }

  let get_neighbor_states (t : t) : t list =
    let neighbors_from_pos pos =
      Coordinate.neighbors pos
      |> List.filter ~f:(fun next_pos -> viable_pair t pos next_pos)
      |> List.map ~f:(fun next_pos -> move_data t pos next_pos)
    in
    Coordinate.neighbors t.empty_space
    |> List.map ~f:neighbors_from_pos
    |> List.join
end

module Nodes_with_cost = struct
  module T = struct
    type t = int * int * Nodes.t [@@deriving sexp_of]

    let hash (_, _, nodes) = Nodes.hash nodes
    let hash_fold_t a (_, _, nodes) = Nodes.hash_fold_t a nodes

    let get_cost
        ({ board; goal_data_pos = (x, y) as pos; empty_space } : Nodes.t) steps
        =
      let dist = x + y in
      let start_used, start_avail = Map.find_exn board (0, 0) in
      let goal_used, _ = Map.find_exn board (x, y) in
      if goal_used > start_used + start_avail then Int.max_value
      else
        let free_space_cost = if start_avail >= goal_used then 0 else 7 in
        let n = Coordinate.distance pos empty_space in
        (dist * 5) + free_space_cost + (goal_used / 4) + (steps * 6) + n

    let get_neighbor_states (_, steps, nodes) =
      let next_nodes = Nodes.get_neighbor_states nodes in
      let steps = steps + 1 in
      List.filter_map next_nodes ~f:(fun node ->
          let cost = get_cost node steps in
          if cost = Int.max_value then None else Some (cost, steps, node))

    let compare (c1, _, _) (c2, _, _) = Int.compare c1 c2
  end

  include T
  include Directed_graph.Extend (T)
end

let part1 s =
  let nodes = Nodes.of_string s in
  let points = Map.keys nodes.board in
  List.cartesian_product points points
  |> List.count ~f:(fun (n1, n2) -> Nodes.viable_pair nodes n1 n2)
  |> Int.to_string |> Ok

let part2 s =
  let nodes = Nodes.of_string s in
  let is_goal (_, _, ({ goal_data_pos; _ } : Nodes.t)) =
    Coordinate.equal goal_data_pos (0, 0)
  in
  let _, res, _ =
    Nodes_with_cost.djikstra
      (Nodes_with_cost.get_cost nodes 0, 0, nodes)
      ~is_goal
  in
  Ok (Int.to_string res)
