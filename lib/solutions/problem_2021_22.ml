open! Core

(* module Amphipod = struct
  type t = No | A | B | C | D [@@deriving equal]

  let to_int = function No -> 0 | A -> 1 | B -> 2 | C -> 3 | D -> 4
  let of_int = function 1 -> A | 2 -> B | 3 -> C | 4 -> D | _ -> No
  let energy_cost = function A -> 1 | B -> 10 | C -> 100 | D -> 1000 | No -> 0
  let goal_room amph = to_int amph - 1
end

module Pos = struct
  module T = struct
    module U = struct
      type t = Hallway of int | Room of int * bool
      [@@deriving compare, equal, hash, sexp]
    end

    include U
    include Comparator.Make (U)
  end

  include T

  let to_int = function
    | Hallway n -> n
    | Room (room_num, deep) -> 11 + (2 * room_num) + Bool.to_int deep

  let of_int n =
    if Int.between n ~low:0 ~high:10 then Hallway n
    else Room ((n - 11) / 2, not (n % 2 = 1))

  let all = List.map (List.range 0 19) ~f:of_int

  let neighbors pos =
    let hallway_neighbors =
      match pos with
      | Hallway 0 -> [ Hallway 1 ]
      | Hallway 10 -> [ Hallway 9 ]
      | Hallway n -> [ Hallway (n - 1); Hallway (n + 1) ]
      | Room (n, false) -> [ Hallway ((n * 2) + 2) ]
      | _ -> []
    in
    let room_neighbors =
      match pos with
      | Hallway 2 -> [ Room (0, false) ]
      | Hallway 4 -> [ Room (1, false) ]
      | Hallway 6 -> [ Room (2, false) ]
      | Hallway 8 -> [ Room (3, false) ]
      | Room (n, deep) -> [ Room (n, not deep) ]
      | _ -> []
    in
    Set.of_list (module T) (hallway_neighbors @ room_neighbors)
end

module Burrow = struct
  type t = int

  let get t pos = t / Int.pow 5 (Pos.to_int pos) % 5 |> Amphipod.of_int

  let set t pos amph =
    let amph = Amphipod.to_int amph in
    let cur_val = get t pos |> Amphipod.to_int in
    let dif = cur_val - amph in
    t + (Int.pow 5 (Pos.to_int pos) * dif)

  let all_places_can_go t pos =
    let visited = Hashtbl.create (module Pos) in
    let rec aux level steps =
      if List.length level = 0 then ()
      else
        let next_steps =
          List.map level ~f:Pos.neighbors
          |> Set.union_list (module Pos)
          |> Set.to_list
          |> List.filter ~f:(fun pos -> not (Hashtbl.mem visited pos))
          |> List.filter ~f:(fun pos ->
                 match get t pos with No -> true | _ -> false)
        in
        List.iter next_steps ~f:(fun key ->
            Hashtbl.add_exn visited ~key ~data:steps);
        aux next_steps (steps + 1)
    in
    aux [ pos ] 1;
    Hashtbl.remove visited pos;
    Hashtbl.to_alist visited

  let valid_end_pos_from_hallway t amph end_pos =
    match end_pos with
    | Pos.Room (n, true) -> n = Amphipod.goal_room amph
    | Room (n, false) ->
        n = Amphipod.goal_room amph
        &&
        let deep_amph = get t (Room (n, true)) in
        Amphipod.equal amph deep_amph || Amphipod.equal deep_amph No
    | _ -> false

  let valid_end_pos_from_room end_pos =
    match end_pos with
    | Pos.Room _ -> false
    | Hallway n ->
        not
          (List.exists [ 2; 4; 6; 8 ] ~f:(fun hallway_num -> n <> hallway_num))

  let places_can_go t pos =
    all_places_can_go t pos
    |> List.filter ~f:(fun (end_pos, _steps) ->
           let amph = get t pos in
           match pos with
           | Room _ -> valid_end_pos_from_room end_pos
           | Hallway _ -> valid_end_pos_from_hallway t amph end_pos)

  let all_moves t =
    List.map Pos.all ~f:(fun start_pos ->
        match get t start_pos with
        | No -> []
        | _ ->
            List.map (places_can_go t start_pos) ~f:(fun (end_pos, steps) ->
                (start_pos, end_pos, steps)))
    |> List.join

  let apply_move t start_pos end_pos =
      let amph = get t start_pos in
      let t = set t start_pos No in
      set t end_pos amph
end


module State = struct
    type t = int * int
    let compare (energy1, _) (energy2, _) = Int.compare energy1 energy2
end *)

let part1 _ = Error (Error.of_string "Unimplemented")

let part2 _ = Error (Error.of_string "Unimplemented")
