open! Core
open! Helpers

module Node : sig
  type t

  val create : size:int -> used:int -> t

  (*= val size : t -> int
  val used : t -> int *)
  val viable_pair : t -> t -> bool
end = struct
  type t =
    { size : int
    ; used : int
    }
  [@@deriving fields ~getters]

  let create ~size ~used = { size; used }
  let avail { size; used } = size - used
  let can_move ~from:{ used; size = _ } ~to_ = used <= avail to_
  let viable_pair ({ used = u1; size = _ } as t1) t2 = u1 > 0 && can_move ~from:t1 ~to_:t2
end

module Board : sig
  type t

  val parse : string -> t
  val node_at : t -> Coordinate.t -> Node.t
  val coordinates : t -> Set.M(Coordinate).t
  (*= val in_bounds : t -> Coordinate.t -> bool *)
end = struct
  type t = Node.t Map.M(Coordinate).t

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
  (*= let in_bounds = Map.mem *)
end

(*= module Pos_and_size = struct
  type t =
    { pos : Coordinate.t
    ; size : int
    }
  [@@deriving compare, hash, sexp]
end

let h_cache = Hashtbl.create (module Pos_and_size)

let h ~board ~from ~to_ =
    let module M = struct
    type t =
      { steps_taken : int
      ; pos : Coordinate.t
      }
    [@@deriving sexp_of]

    let compare { steps_taken = n1; pos = p1 } { steps_taken = n2; pos = p2 } =
      match n1 = n2 with
      | true -> [%compare: Coordinate.t] p1 p2
      | false -> [%compare: int] n1 n2
    ;;
  end
  in
  let module Pq = Priority_queue.Make (M) in
  let size = Board.node_at board from |> Node.size in
  let rec aux pq visited =
    let%map.Option ({ steps_taken; pos }, pq) = Pq.get pq in
    Hashtbl.set h_cache ~key:{pos; size} ~data:steps_taken;
    match [%equal: Coordinate.t] pos from with
    | true -> steps_taken
    | false -> 
            (
                match Hashtbl.find h_cache ~key:(
            )
  in
  aux
    (Pq.singleton { steps_taken = 0; pos = from })
    (Map.singleton (module Coordinate) from 0) 
;;  *)

let part1 s =
  let board = Board.parse s in
  let coords = Board.coordinates board |> Set.to_list in
  List.cartesian_product coords coords
  |> List.count ~f:(fun (p1, p2) ->
    Node.viable_pair (Board.node_at board p1) (Board.node_at board p2))
  |> Int.to_string
;;

let part2 _ = failwith ""
