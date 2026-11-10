(* i hate aoc problems like this, i've written a general solution that is way too slow 

   but i've looked at the input and we can just reduce it to full nodes, empty nodes, and
   nodes that we can't do anything with (i.e. they hold more data than they could transfer
   to any other node and don't have enough empty space that another node could transfer
   into it. that transforms this problem into just like a 16 puzzle or whatever it's
   called. *)
open! Core
open! Helpers

module Node : sig
  type t [@@deriving compare, sexp]

  val create : size:int -> used:int -> t
  val size : t -> int
  val used : t -> int
  val viable_pair : t -> t -> bool
  val avail : t -> int
end = struct
  type t =
    { size : int
    ; used : int
    }
  [@@deriving compare, fields ~getters, sexp]

  let create ~size ~used =
    if used > size then failwith "[used] can't be greater than [size]";
    { size; used }
  ;;

  let avail { size; used } = size - used
  let can_move ~from:{ used; size = _ } ~to_ = used <= avail to_
  let viable_pair ({ used = u1; size = _ } as t1) t2 = u1 > 0 && can_move ~from:t1 ~to_:t2
end

module Board : sig
  type t [@@deriving compare, sexp]

  val parse : string -> t
  val node_at : t -> Coordinate.t -> Node.t
  val coordinates : t -> Set.M(Coordinate).t
  val in_bounds : t -> Coordinate.t -> bool
  val move : t -> from:Coordinate.t -> to_:Coordinate.t -> t
end = struct
  type t = Node.t Map.M(Coordinate).t [@@deriving compare, sexp]

  let parse s =
    let lines = String.split_lines s in
    List.drop lines 2
    |> List.fold
         ~init:(Map.empty (module Coordinate))
         ~f:(fun acc line ->
           match Parse.line_numbers line with
           | [ x; y; size; used; _avail; _use_percent ] ->
             Map.add_exn acc ~key:(x, y) ~data:(Node.create ~size ~used)
           | _ -> failwith "bad number of numbers in line")
  ;;

  let node_at = Map.find_exn
  let coordinates = Map.key_set
  let in_bounds = Map.mem

  let move t ~from ~to_ =
    let from_n = node_at t from in
    let to_n = node_at t to_ in
    let t = Map.set t ~key:from ~data:(Node.create ~used:0 ~size:(Node.size from_n)) in
    Map.set
      t
      ~key:to_
      ~data:(Node.create ~used:(Node.used from_n + Node.used to_n) ~size:(Node.size to_n))
  ;;
end

module Pos_and_size = struct
  type t =
    { pos : Coordinate.t
    ; size : int
    }
  [@@deriving compare, hash, sexp]
end

let h' ~from ~to_ = Coordinate.distance from to_

(* apply 1 step penalty if the neighbor has capacity but will need to shuffle stuff *)
let h ~board ~from ~to_ =
  let used = Board.node_at board from |> Node.used in
  let module M = struct
    type t =
      { steps_taken : int
      ; pos : Coordinate.t
      }
    [@@deriving sexp_of]

    let compare { steps_taken = n1; pos = p1 } { steps_taken = n2; pos = p2 } =
      let lb1 = n1 + h' ~from:p1 ~to_ in
      let lb2 = n2 + h' ~from:p2 ~to_ in
      match lb1 = lb2 with
      | true -> [%compare: int * Coordinate.t] (n1, p1) (n2, p2)
      | false -> [%compare: int] lb1 lb2
    ;;
  end
  in
  let module Pq = Priority_queue.Make (M) in
  let rec aux pq visited =
    let%bind.Option { steps_taken; pos }, pq = Pq.get pq in
    match [%equal: Coordinate.t] pos to_ with
    | true -> Some steps_taken
    | false ->
      (match Set.mem visited pos with
       | true -> aux pq visited
       | false ->
         let visited = Set.add visited pos in
         let pq =
           Coordinate.neighbors pos
           |> List.filter ~f:(Board.in_bounds board)
           |> List.filter ~f:(fun pos -> (* is this valid? *) not (Set.mem visited pos))
           |> List.fold ~init:pq ~f:(fun acc pos ->
             let steps_to_neighbor =
               let neighbor_node = Board.node_at board pos in
               match Node.avail neighbor_node >= used with
               | true -> Some 1
               | false ->
                 (match Node.size neighbor_node >= used with
                  | true -> Some 2
                  | false -> None)
             in
             match steps_to_neighbor with
             | None -> acc
             | Some steps_to_neighbor ->
               Pq.add acc { pos; steps_taken = steps_taken + steps_to_neighbor })
         in
         aux pq visited)
  in
  aux (Pq.singleton { steps_taken = 0; pos = from }) (Set.empty (module Coordinate))
;;

let solve2 ~board ~from ~to_:end_goal =
  let all_positions = Board.coordinates board |> Set.to_list in
  let all_moves =
    List.map all_positions ~f:(fun pos ->
      Coordinate.neighbors pos
      |> List.filter ~f:(Board.in_bounds board)
      |> List.map ~f:(fun to_ -> pos, to_))
    |> List.join
  in
  let possible_moves board =
    List.filter all_moves ~f:(fun (from, to_) ->
      let from_n = Board.node_at board from in
      let to_n = Board.node_at board to_ in
      Node.used from_n > 0 && Node.used from_n <= Node.avail to_n)
  in
  let module State = struct
    module T = struct
      type t =
        { board : Board.t
        ; valuable_pos : Coordinate.t
        }
      [@@deriving compare, sexp]
    end

    include Comparator.Make (T)
    include T
  end
  in
  let module M = struct
    type t =
      { steps_taken : int
      ; h_val : int
      ; state : State.t
      }
    [@@deriving sexp_of]

    let compare
          { steps_taken = st1; h_val = h1; state = s1 }
          { steps_taken = st2; h_val = h2; state = s2 }
      =
      let lb1 = st1 + h1 in
      let lb2 = st2 + h2 in
      match lb1 = lb2 with
      | true -> [%compare: int * int * State.t] (st1, h1, s1) (st2, h2, s2)
      | false -> [%compare: int] lb1 lb2
    ;;
  end
  in
  let module Pq = Priority_queue.Make (M) in
  let rec aux pq visited =
    let ({ steps_taken; state = { valuable_pos; board } as state; h_val = _ } : M.t), pq =
      Pq.get_exn pq
    in
    match [%equal: Coordinate.t] valuable_pos end_goal with
    | true -> steps_taken
    | false ->
      (match Set.mem visited state with
       | true -> aux pq visited
       | false ->
         let visited = Set.add visited state in
         let neighbor_states =
           possible_moves board
           |> List.filter_map ~f:(fun (from, to_) ->
             let board = Board.move board ~from ~to_ in
             let valuable_pos =
               match [%equal: Coordinate.t] from valuable_pos with
               | true -> to_
               | false -> valuable_pos
             in
             let state = ({ board; valuable_pos } : State.t) in
             match Set.mem visited state with
             | true -> None
             | false ->
               let%map.Option h_val = h ~board ~from:valuable_pos ~to_:end_goal in
               ({ steps_taken = steps_taken + 1; h_val; state } : M.t))
         in
         let pq = List.fold neighbor_states ~init:pq ~f:Pq.add in
         aux pq visited)
  in
  aux
    (Pq.singleton
       { steps_taken = 0
       ; h_val = h ~board ~from ~to_:end_goal |> Option.value_exn
       ; state = { board; valuable_pos = from }
       })
    (Set.empty (module State))
;;

let part1 s =
  let board = Board.parse s in
  let coords = Board.coordinates board |> Set.to_list in
  List.cartesian_product coords coords
  |> List.filter ~f:(fun (p1, p2) -> not ([%equal: Coordinate.t] p1 p2))
  |> List.count ~f:(fun (p1, p2) ->
    Node.viable_pair (Board.node_at board p1) (Board.node_at board p2))
  |> Int.to_string
;;

let part2 s =
  let board = Board.parse s in
  let coords = Board.coordinates board in
  let max_x =
    Set.to_list coords
    |> List.map ~f:(fun (x, _y) -> x)
    |> List.max_elt ~compare:[%compare: Int.t]
    |> Option.value_exn
  in
  solve2 ~board ~from:(max_x, 0) ~to_:(0, 0) |> Int.to_string
;;
