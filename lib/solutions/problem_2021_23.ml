open! Core
open! Helpers

module Amphipod = struct
  type t = No | A | B | C | D [@@deriving equal, sexp]

  let of_char_exn = function
    | 'A' -> A
    | 'B' -> B
    | 'C' -> C
    | 'D' -> D
    | c -> raise_s [%message "Bad character" (c : char)]

  let to_char = function A -> 'A' | B -> 'B' | C -> 'C' | D -> 'D' | No -> '.'
  let to_int = function No -> 0 | A -> 1 | B -> 2 | C -> 3 | D -> 4

  let of_int_exn = function
    | 1 -> A
    | 2 -> B
    | 3 -> C
    | 4 -> D
    | 0 -> No
    | n -> raise_s [%message "Bad amphipod int" (n : int)]

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
  type t = int [@@deriving sexp]

  let get t pos = t / Int.pow 5 (Pos.to_int pos) % 5 |> Amphipod.of_int_exn

  let set t pos amph =
    let amph = Amphipod.to_int amph in
    let cur_val = get t pos |> Amphipod.to_int in
    let dif = amph - cur_val in
    t + (Int.pow 5 (Pos.to_int pos) * dif)

  let goal_state =
    List.fold
      [
        (Pos.Room (0, false), Amphipod.A);
        (Pos.Room (0, false), A);
        (Pos.Room (1, false), B);
        (Pos.Room (1, false), B);
        (Pos.Room (2, false), C);
        (Pos.Room (2, false), C);
        (Pos.Room (3, false), D);
        (Pos.Room (3, false), D);
      ]
      ~init:0
      ~f:(fun acc (pos, amph) -> set acc pos amph)

  let of_string s =
    let positions =
      [
        (3, 2, Pos.Room (0, false));
        (5, 2, Room (1, false));
        (7, 2, Room (2, false));
        (9, 2, Room (3, false));
        (3, 3, Room (0, true));
        (5, 3, Room (1, true));
        (7, 3, Room (2, true));
        (9, 3, Room (3, true));
      ]
    in
    let s = String.split_lines s |> List.to_array in
    List.fold positions ~init:0 ~f:(fun acc (col, row, pos) ->
        set acc pos (Amphipod.of_char_exn (String.get s.(row) col)))

  let to_string t =
    let hallway =
      List.map (List.range 0 11) ~f:(fun i ->
          get t (Pos.Hallway i) |> Amphipod.to_char)
      |> String.of_list
    in
    let rooms1 =
      List.map (List.range 0 4) ~f:(fun i ->
          [ get t (Pos.Room (i, false)) |> Amphipod.to_char; ' ' ])
      |> List.join |> String.of_list
    in
    let rooms2 =
      List.map (List.range 0 4) ~f:(fun i ->
          [ get t (Pos.Room (i, true)) |> Amphipod.to_char; ' ' ])
      |> List.join |> String.of_list
    in
    hallway ^ "\n  " ^ rooms1 ^ "\n  " ^ rooms2

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
        Amphipod.equal amph deep_amph && (Amphipod.goal_room deep_amph = n)
    | _ -> false

  let valid_end_pos_from_room end_pos =
    match end_pos with
    | Pos.Room _ -> false
    | Hallway n -> List.for_all [ 2; 4; 6; 8 ] ~f:(fun n2 -> n <> n2)

  let places_can_go t pos =
    all_places_can_go t pos
    |> List.filter ~f:(fun (end_pos, _steps) ->
           let amph = get t pos in
           match pos with
           | Room _ -> valid_end_pos_from_room end_pos
           | Hallway _ -> valid_end_pos_from_hallway t amph end_pos)

  let all_moves (t : t) : (Pos.t * Pos.t * int) list =
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

  let move_cost t start_pos steps =
    let amph = get t start_pos in
    Amphipod.energy_cost amph * steps
end

module State = struct
  type t = int * Burrow.t [@@deriving sexp]

  let compare (energy1, _) (energy2, _) = Int.compare energy1 energy2
end

module Pq = Priority_queue.Make (State)

let djikstra t =
  let rec aux pq =
    let (energy, pos), pq = Pq.get_exn pq in
    if pos = Burrow.goal_state then energy
    else
      let pq =
        List.fold (Burrow.all_moves pos) ~init:pq
          ~f:(fun acc (start_, end_, steps) ->
            let new_state = Burrow.apply_move pos start_ end_ in
            Pq.add acc (energy + Burrow.move_cost pos start_ steps, new_state))
      in
      aux pq
  in
  aux (Pq.singleton (0, t))

let part1 s =
  let burrow = Burrow.of_string s in
  djikstra burrow |> Int.to_string |> Ok

let part2 _ = Error (Error.of_string "Unimplemented")

let%expect_test "" =
  let t = 0 in
  let pos = Pos.Hallway 0 in
  let t = Burrow.set t pos Amphipod.A in
  print_s [%sexp (Burrow.get t pos : Amphipod.t)];
  [%expect {| A |}];
  let pos = Pos.Room (3, true) in
  let t = Burrow.set t pos Amphipod.B in
  print_s [%sexp (Burrow.get t pos : Amphipod.t)];
  [%expect {| B |}];
  ()

let test_string =
  {|#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########|}

let%expect_test "" =
  let burrow = Burrow.of_string test_string in
  print_endline (Burrow.to_string burrow);
  [%expect {|
    ...........
      B C B D
      A D C A
    |}];
  ()

let%expect_test "places can go" =
  let burrow = Burrow.of_string test_string in
  print_s [%sexp (List.length (Burrow.all_moves burrow) : int)];
  [%expect {| 28 |}];
  let burrow = Burrow.apply_move burrow (Pos.Room (0, false)) (Pos.Hallway 0) in
  print_endline (Burrow.to_string burrow);
  [%expect {||}];
  print_s [%sexp (List.length (Burrow.all_moves burrow) : int)];
  [%expect {| 18 |}];
  ()
