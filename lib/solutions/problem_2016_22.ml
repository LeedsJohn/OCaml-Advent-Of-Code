(* i hate aoc problems like this, i've written a general solution that is way too slow 

   but i've looked at the input and we can just reduce it to full nodes, empty nodes, and
   nodes that we can't do anything with (i.e. they hold more data than they could transfer
   to any other node and don't have enough empty space that another node could transfer
   into it. that transforms this problem into just like a 16 puzzle or whatever it's
   called. *)
open! Core
open! Helpers

module Coord_pair : sig
  type t [@@deriving compare, equal, hash, sexp]

  val create : Coordinate.t -> Coordinate.t -> t
end = struct
  type t = Coordinate.t * Coordinate.t [@@deriving compare, equal, hash, sexp]

  let create c1 c2 =
    match [%compare: Coordinate.t] c1 c2 |> Ordering.of_int with
    | Less | Equal -> c1, c2
    | Greater -> c2, c1
  ;;
end

module Distance_cache : sig
  val get : Coordinate.t * Coordinate.t -> int option
  val set : Coordinate.t * Coordinate.t -> int -> unit
end = struct
  let the_thing = Hashtbl.create (module Coord_pair)
  let get (c1, c2) = Hashtbl.find the_thing (Coord_pair.create c1 c2)
  let set (c1, c2) n = Hashtbl.set the_thing ~key:(Coord_pair.create c1 c2) ~data:n
end

module Board : sig
  module State : sig
    type t [@@deriving compare, sexp]

    include Comparator.S with type t := t
  end

  type t [@@deriving compare, sexp]

  val state : t -> State.t
  val parse : string -> t
  val all_moves : t -> (Coordinate.t * Coordinate.t) list
  val move : t -> from:Coordinate.t -> to_:Coordinate.t -> t
  val valuable_pos : t -> Coordinate.t
  val empty_pos : t -> Coordinate.t
  val distance : t -> Coordinate.t -> Coordinate.t -> int
end = struct
  module State = struct
    module T = struct
      type t =
        { valuable_pos : Coordinate.t
        ; empty_pos : Coordinate.t
        }
      [@@deriving compare, fields ~getters, sexp]
    end

    include Comparator.Make (T)
    include T
  end

  type t =
    { max_x : int
    ; max_y : int
    ; dead_spots : Set.M(Coordinate).t
    ; state : State.t
    }
  [@@deriving compare, fields ~getters, sexp]

  let valuable_pos = Fn.compose State.valuable_pos state
  let empty_pos = Fn.compose State.empty_pos state

  let parse s =
    let lines = String.split_lines s in
    let dead_spots, max_x, max_y, empty_pos =
      List.drop lines 2
      |> List.fold
           ~init:(Set.empty (module Coordinate), 0, 0, Coordinate.origin)
           ~f:(fun (dead_spots, max_x, max_y, empty_pos) line ->
             match Parse.line_numbers line with
             | [ x; y; _size; used; _avail; _use_percent ] ->
               let max_x = Int.max max_x x in
               let max_y = Int.max max_y y in
               (match used >= 100 with
                | true ->
                  Set.add dead_spots (Coordinate.make ~x ~y), max_x, max_y, empty_pos
                | false ->
                  (match used = 0 with
                   | true -> dead_spots, max_x, max_y, Coordinate.make ~x ~y
                   | false -> dead_spots, max_x, max_y, empty_pos))
             | _ -> failwith "bad number of numbers in line")
    in
    let (state : State.t) = { valuable_pos = Coordinate.make ~x:max_x ~y:0; empty_pos } in
    { max_x; max_y; dead_spots; state }
  ;;

  let valid_pos { max_x; max_y; dead_spots; state = _ } pos =
    let x, y = Coordinate.xy pos in
    Int.between x ~low:0 ~high:max_x
    && Int.between y ~low:0 ~high:max_y
    && not (Set.mem dead_spots pos)
  ;;

  let valid_move t ~from ~to_ =
    valid_pos t from
    && valid_pos t to_
    && (not ([%equal: Coordinate.t] from to_))
    && [%equal: Coordinate.t] to_ (empty_pos t)
  ;;

  let move t ~from ~to_ =
    if not (valid_move t ~from ~to_) then failwith "invalid move";
    match [%equal: Coordinate.t] from (valuable_pos t) with
    | true -> { t with state = { valuable_pos = to_; empty_pos = from } }
    | false -> { t with state = { empty_pos = from; valuable_pos = valuable_pos t } }
  ;;

  let all_moves t =
    let empty_pos = empty_pos t in
    Coordinate.neighbors empty_pos
    |> List.filter ~f:(fun from -> valid_move t ~to_:empty_pos ~from)
    |> List.map ~f:(fun from -> from, empty_pos)
  ;;

  let distance t start_pos end_pos =
    let seen_this_time = Hash_set.create (module Coordinate) in
    let rec aux q =
      let (pos, steps), q = Fqueue.dequeue_exn q in
      match Distance_cache.get (pos, end_pos) with
      | Some n -> steps + n
      | None ->
        Coordinate.neighbors pos
        |> List.filter ~f:(valid_pos t)
        |> List.filter ~f:(fun pos -> not (Hash_set.mem seen_this_time pos))
        |> List.fold ~init:q ~f:(fun acc next_pos ->
          Distance_cache.set (start_pos, next_pos) (steps + 1);
          Hash_set.add seen_this_time next_pos;
          Fqueue.enqueue acc (next_pos, steps + 1))
        |> aux
    in
    Hash_set.add seen_this_time start_pos;
    Distance_cache.set (start_pos, start_pos) 0;
    Distance_cache.set (end_pos, end_pos) 0;
    aux (Fqueue.singleton (start_pos, 0))
  ;;
end

let goal_pos = Coordinate.origin

let h board =
  let empty_pos = Board.empty_pos board in
  let valuable_pos = Board.valuable_pos board in
  match [%equal: Coordinate.t] valuable_pos goal_pos with
  | true -> 0
  | false ->
    Board.distance board empty_pos valuable_pos
    - 1
    + Board.distance board valuable_pos goal_pos
;;

let solve board =
  let module M = struct
    type t =
      { board : Board.t
      ; h_val : int
      ; steps : int
      }
    [@@deriving sexp_of]

    let compare
          { board = b1; h_val = h1; steps = s1 }
          { board = b2; h_val = h2; steps = s2 }
      =
      let lb1 = h1 + s1 in
      let lb2 = h2 + s2 in
      match lb1 = lb2 with
      | true -> [%compare: int] lb1 lb2
      | false -> [%compare: int * int * Board.t] (s1, h1, b1) (s2, h2, b2)
    ;;
  end
  in
  let module Pq = Priority_queue.Make (M) in
  let rec aux pq visited =
    let ({ board; steps; h_val = _ } : M.t), pq = Pq.get_exn pq in
    let valuable_pos = Board.valuable_pos board in
    let state = Board.state board in
    match [%equal: Coordinate.t] valuable_pos goal_pos with
    | true -> steps
    | false ->
      (match Set.mem visited state with
       | true -> aux pq visited
       | false ->
         let visited = Set.add visited state in
         let neighbor_boards =
           Board.all_moves board
           |> List.filter_map ~f:(fun (from, to_) ->
             let new_board = Board.move board ~from ~to_ in
             let new_state = Board.state new_board in
             match Set.mem visited new_state with
             | true -> None
             | false ->
               let h_val = steps + h new_board in
               Some ({ board = new_board; h_val; steps = steps + 1 } : M.t))
         in
         let pq = List.fold neighbor_boards ~init:pq ~f:Pq.add in
         aux pq visited)
  in
  aux
    (Pq.singleton { steps = 0; h_val = h board; board })
    (Set.empty (module Board.State))
;;

let part2 s =
  let board = Board.parse s in
  solve board |> Int.to_string
;;

module Node2 : sig
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

module Board2 : sig
  type t [@@deriving compare, sexp]

  val parse : string -> t
  val node_at : t -> Coordinate.t -> Node2.t
  val coordinates : t -> Set.M(Coordinate).t
  val in_bounds : t -> Coordinate.t -> bool
  val move : t -> from:Coordinate.t -> to_:Coordinate.t -> t
end = struct
  type t = Node2.t Map.M(Coordinate).t [@@deriving compare, sexp]

  let parse s =
    let lines = String.split_lines s in
    List.drop lines 2
    |> List.fold
         ~init:(Map.empty (module Coordinate))
         ~f:(fun acc line ->
           match Parse.line_numbers line with
           | [ x; y; size; used; _avail; _use_percent ] ->
             Map.add_exn acc ~key:(Coordinate.make ~x ~y) ~data:(Node2.create ~size ~used)
           | _ -> failwith "bad number of numbers in line")
  ;;

  let node_at = Map.find_exn
  let coordinates = Map.key_set
  let in_bounds = Map.mem

  let move t ~from ~to_ =
    let from_n = node_at t from in
    let to_n = node_at t to_ in
    let t = Map.set t ~key:from ~data:(Node2.create ~used:0 ~size:(Node2.size from_n)) in
    Map.set
      t
      ~key:to_
      ~data:
        (Node2.create ~used:(Node2.used from_n + Node2.used to_n) ~size:(Node2.size to_n))
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
  let used = Board2.node_at board from |> Node2.used in
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
           |> List.filter ~f:(Board2.in_bounds board)
           |> List.filter ~f:(fun pos -> (* is this valid? *) not (Set.mem visited pos))
           |> List.fold ~init:pq ~f:(fun acc pos ->
             let steps_to_neighbor =
               let neighbor_node = Board2.node_at board pos in
               match Node2.avail neighbor_node >= used with
               | true -> Some 1
               | false ->
                 (match Node2.size neighbor_node >= used with
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
  let all_positions = Board2.coordinates board |> Set.to_list in
  let all_moves =
    List.map all_positions ~f:(fun pos ->
      Coordinate.neighbors pos
      |> List.filter ~f:(Board2.in_bounds board)
      |> List.map ~f:(fun to_ -> pos, to_))
    |> List.join
  in
  let possible_moves board =
    List.filter all_moves ~f:(fun (from, to_) ->
      let from_n = Board2.node_at board from in
      let to_n = Board2.node_at board to_ in
      Node2.used from_n > 0 && Node2.used from_n <= Node2.avail to_n)
  in
  let module State = struct
    module T = struct
      type t =
        { board : Board2.t
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
             let board = Board2.move board ~from ~to_ in
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
  let board = Board2.parse s in
  let coords = Board2.coordinates board |> Set.to_list in
  List.cartesian_product coords coords
  |> List.filter ~f:(fun (p1, p2) -> not ([%equal: Coordinate.t] p1 p2))
  |> List.count ~f:(fun (p1, p2) ->
    Node2.viable_pair (Board2.node_at board p1) (Board2.node_at board p2))
  |> Int.to_string
;;

let _old_part2 s =
  let board = Board2.parse s in
  let coords = Board2.coordinates board in
  let max_x =
    Set.to_list coords
    |> List.map ~f:Coordinate.x
    |> List.max_elt ~compare:[%compare: Int.t]
    |> Option.value_exn
  in
  solve2 ~board ~from:(Coordinate.make ~x:max_x ~y:0) ~to_:Coordinate.origin
  |> Int.to_string
;;
