open! Core
open! Helpers

(*
   part 1:

#############
#...........#
###D#C#D#B###
  #B#A#A#C#
  #A#B#C#D#
  #A#B#C#D#
  #########

  part 2:
#############
#...........#
###D#C#D#B###
  #D#C#B#A#
  #D#B#A#C#
  #B#A#A#C#
  #########

  test input 1:

#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #A#B#C#D#
  #A#B#C#D#
  #########

  test input 2:
#############
#...........#
###B#C#B#D###
  #D#C#B#A#
  #D#B#A#C#
  #A#D#C#A#
  #########
*)

let part1_rooms = [ "DBAA"; "CABB"; "DACC"; "BCDD" ]
let part2_rooms = [ "DDDB"; "CCBA"; "DBAA"; "BACC" ]
let test1_rooms = [ "BAAA"; "CDBB"; "BCCC"; "DADD" ]
let test2_rooms = [ "BDDA"; "CCBD"; "BBAC"; "DACA" ]
let num_positions = 11 + (4 * 4)

module Amphipod = struct
  type t =
    | A
    | B
    | C
    | D
  [@@deriving equal]

  let energy_per_move = function
    | A -> 1
    | B -> 10
    | C -> 100
    | D -> 1000
  ;;

  let to_int = function
    | A -> 0
    | B -> 1
    | C -> 2
    | D -> 3
  ;;

  let of_char = function
    | 'A' -> A
    | 'B' -> B
    | 'C' -> C
    | 'D' -> D
    | _ -> failwith "bad amphipod char"
  ;;

  let to_char = function
    | A -> 'A'
    | B -> 'B'
    | C -> 'C'
    | D -> 'D'
  ;;
end

module Position : sig
  module Spec : sig
    type t =
      | Hallway of int
      | Room of
          { room_index : int
          ; depth : int
          }
  end

  type t = private int [@@deriving sexp]

  val of_spec : Spec.t -> t
  val to_spec : t -> Spec.t
  val to_bitmask : t -> int
  val to_index : t -> int
  val of_index : int -> t
  val is_hallway : t -> bool
  val distance_if_no_blockers : t -> t -> int
end = struct
  module Spec = struct
    type t =
      | Hallway of int
      | Room of
          { room_index : int
          ; depth : int
          }
    [@@deriving sexp]
  end

  type t = int

  let max_room_depth = 4
  let hallway_length = 11
  let is_hallway t = Int.bit_and t 0b11111111111 <> 0

  let of_spec : Spec.t -> t = function
    | Hallway n -> 1 lsl n
    | Room { room_index; depth } ->
      Int.shift_left 1 (hallway_length + (max_room_depth * room_index) + depth)
  ;;

  let to_spec t : Spec.t =
    match is_hallway t with
    | true -> Hallway (Int.ctz t)
    | false ->
      let index = Int.ctz t - hallway_length in
      let room_index = index / 4 in
      let depth = index % 4 in
      Room { room_index; depth }
  ;;

  let sexp_of_t t = to_spec t |> [%sexp_of: Spec.t]
  let t_of_sexp s = [%of_sexp: Spec.t] s |> of_spec

  let distance_if_no_blockers t1 t2 =
    let spec1 = to_spec t1 in
    let spec2 = to_spec t2 in
    let hallway_room_dif ~hallway_index ~room_index ~depth =
      let room_hallway_index = 2 + (room_index * 2) in
      1 + depth + Int.abs (hallway_index - room_hallway_index)
    in
    match spec1, spec2 with
    | Hallway i, Hallway j -> Int.abs (i - j)
    | Room { room_index; depth }, Hallway hallway_index
    | Hallway hallway_index, Room { room_index; depth } ->
      hallway_room_dif ~hallway_index ~room_index ~depth
    | Room { room_index = ri1; depth = d1 }, Room { room_index = ri2; depth = d2 } ->
      (match ri1 = ri2 with
       | true -> Int.abs (d1 - d2)
       | false -> 2 + d1 + d2 + (2 * Int.abs (ri1 - ri2)))
  ;;

  let to_bitmask = Fn.id
  let to_index = Int.ctz
  let of_index x = Int.shift_left 1 x
end

module Position_list : sig
  type t = private int [@@deriving compare, equal, sexp]

  val empty : t
  val singleton : Position.t -> t
  val set : t -> Position.t -> t
  val clear : t -> Position.t -> t
  val mem : t -> Position.t -> bool

  (* Folds rooms from top to bottom (i hope). *)
  val fold : t -> init:'a -> f:('a -> Position.t -> 'a) -> 'a
  val to_list : t -> Position.t list
  val remove : t -> remove_these:t -> t
  val union : t -> t -> t
  val intersect : t -> t -> t
  val room_for_amphipod : Amphipod.t -> t
  val hallways_not_in_front_of_room : t
end = struct
  type t = int [@@deriving compare, equal, sexp]

  let empty = 0
  let set t pos = Int.bit_or t (Position.to_bitmask pos)
  let clear t pos = Int.bit_and t (Int.bit_not (Position.to_bitmask pos))
  let mem t pos = Int.bit_and t (Position.to_bitmask pos) <> 0

  let rec fold' t acc ~f ~count =
    match t = 0 with
    | true -> acc
    | false ->
      (match Int.bit_and t 1 = 1 with
       | true ->
         fold'
           (Int.shift_right t 1)
           (f acc (Position.of_index count))
           ~f
           ~count:(count + 1)
       | false -> fold' (Int.shift_right t 1) acc ~f ~count:(count + 1))
  ;;

  let fold t ~init ~f = fold' t init ~f ~count:0
  let to_list t = fold t ~init:[] ~f:(fun acc pos -> pos :: acc)
  let of_list = List.fold ~init:empty ~f:set
  let singleton pos = of_list [ pos ]
  let remove t ~remove_these = Int.bit_and t (Int.bit_not remove_these)
  let union = Int.bit_or
  let intersect = Int.bit_and

  let make_room_for_amphipod amphipod =
    let room_index = Amphipod.to_int amphipod in
    List.range 0 4
    |> List.fold ~init:empty ~f:(fun acc depth ->
      set acc (Position.of_spec (Room { room_index; depth })))
  ;;

  let a_room = make_room_for_amphipod A
  let b_room = make_room_for_amphipod B
  let c_room = make_room_for_amphipod C
  let d_room = make_room_for_amphipod D

  let room_for_amphipod : Amphipod.t -> t = function
    | A -> a_room
    | B -> b_room
    | C -> c_room
    | D -> d_room
  ;;

  let hallways_not_in_front_of_room =
    List.map [ 0; 1; 3; 5; 7; 9; 10 ] ~f:(fun i -> Position.Spec.Hallway i)
    |> List.map ~f:Position.of_spec
    |> of_list
  ;;
end

module Edges : sig
  val neighbors : Position.t -> Position_list.t
end = struct
  let empty = Array.create ~len:num_positions Position_list.empty

  let add_edge t pos1 pos2 =
    let new_t = Array.copy t in
    new_t.(Position.to_index pos1)
    <- Position_list.set new_t.(Position.to_index pos1) pos2;
    new_t.(Position.to_index pos2)
    <- Position_list.set new_t.(Position.to_index pos2) pos1;
    new_t
  ;;

  let the_one =
    let t =
      List.range 0 10
      |> List.fold ~init:empty ~f:(fun acc i ->
        let pos1 = Position.of_spec (Hallway i) in
        let pos2 = Position.of_spec (Hallway (i + 1)) in
        add_edge acc pos1 pos2)
    in
    let add_room t room_index ~connected_hallway_index =
      let t =
        add_edge
          t
          (Position.of_spec (Hallway connected_hallway_index))
          (Position.of_spec (Room { room_index; depth = 0 }))
      in
      List.range 0 3
      |> List.fold ~init:t ~f:(fun acc depth ->
        let pos1 = Position.of_spec (Room { room_index; depth }) in
        let pos2 = Position.of_spec (Room { room_index; depth = depth + 1 }) in
        add_edge acc pos1 pos2)
    in
    List.range 0 4
    |> List.fold ~init:t ~f:(fun acc room_index ->
      add_room acc room_index ~connected_hallway_index:(2 + (room_index * 2)))
  ;;

  let neighbors pos = the_one.(Position.to_index pos)
end

module Move_type = struct
  type t =
    | Forced of Position.t * Position.t
    | Choices of (Position.t * Position.t) list
end

module Board : sig
  type t [@@deriving compare, equal, sexp]

  include Comparator.S with type t := t

  val make : string list -> t
  val valid_moves : t -> Move_type.t
  val apply_move : t -> from:Position.t -> to_:Position.t -> t
  val is_solved : t -> bool
  val h : t -> int
  val amphipod_at_pos_exn : t -> Position.t -> Amphipod.t
  val print : t -> unit
end = struct
  module T = struct
    type t =
      { a : Position_list.t
      ; b : Position_list.t
      ; c : Position_list.t
      ; d : Position_list.t
      }
    [@@deriving compare, equal, sexp]
  end

  include Comparator.Make (T)
  include T

  let select_pos_list { a; b; c; d } (amphipod : Amphipod.t) =
    match amphipod with
    | A -> a
    | B -> b
    | C -> c
    | D -> d
  ;;

  let make rooms =
    let t =
      { a = Position_list.empty
      ; b = Position_list.empty
      ; c = Position_list.empty
      ; d = Position_list.empty
      }
    in
    List.foldi rooms ~init:t ~f:(fun room_index t amphipods ->
      String.to_list amphipods
      |> List.foldi ~init:t ~f:(fun depth t c ->
        let amph = Amphipod.of_char c in
        let pos = Position.of_spec (Room { room_index; depth }) in
        let pos_list = Position_list.set (select_pos_list t amph) pos in
        match amph with
        | A -> { t with a = pos_list }
        | B -> { t with b = pos_list }
        | C -> { t with c = pos_list }
        | D -> { t with d = pos_list }))
  ;;

  let apply_move { a; b; c; d } ~from ~to_ =
    let f pos_list =
      match Position_list.mem pos_list from with
      | false -> pos_list
      | true ->
        let pos_list = Position_list.clear pos_list from in
        Position_list.set pos_list to_
    in
    { a = f a; b = f b; c = f c; d = f d }
  ;;

  let fold_positions_with_amphipods { a; b; c; d } =
    let pos_list =
      Position_list.union a b |> Position_list.union c |> Position_list.union d
    in
    Position_list.fold pos_list
  ;;

  let amphipod_at_pos { a; b; c; d } pos : Amphipod.t option =
    if Position_list.mem a pos
    then Some A
    else if Position_list.mem b pos
    then Some B
    else if Position_list.mem c pos
    then Some C
    else if Position_list.mem d pos
    then Some D
    else None
  ;;

  let amphipod_at_pos_exn t pos = Option.value_exn (amphipod_at_pos t pos)

  let print t =
    let char_at_pos pos =
      amphipod_at_pos t pos |> Option.value_map ~default:'.' ~f:Amphipod.to_char
    in
    let hallway =
      List.range 0 11
      |> List.map ~f:(fun i -> Position.of_spec (Hallway i) |> char_at_pos)
      |> String.of_list
    in
    let hallway = "#" ^ hallway ^ "#" in
    let room_char room_index depth =
      Position.of_spec (Room { room_index; depth }) |> char_at_pos |> Char.to_string
    in
    let row_string depth =
      let outside2 =
        match depth with
        | 0 -> "##"
        | _ -> "  "
      in
      [%string
        "%{outside2}#%{room_char 0 depth}#%{room_char 1 depth}#%{room_char 2 \
         depth}#%{room_char 3 depth}#%{outside2}"]
    in
    let top = "#############" in
    let bot = "  #########" in
    print_endline top;
    print_endline hallway;
    for i = 0 to 3 do
      row_string i |> print_endline
    done;
    print_endline bot
  ;;

  let num_amphipods_solved t amphipod =
    let room_index = Amphipod.to_int amphipod in
    let rec aux depth res =
      match depth < 0 with
      | true -> res
      | false ->
        (match
           [%equal: Amphipod.t option]
             (amphipod_at_pos t (Position.of_spec (Room { room_index; depth })))
             (Some amphipod)
         with
         | true -> aux (depth - 1) (res + 1)
         | false -> res)
    in
    aux 3 0
  ;;

  let is_pos_solved_amphipod t pos =
    match Position.to_spec pos with
    | Hallway _ -> false
    | Room { depth; room_index } ->
      (match amphipod_at_pos t pos with
       | None -> false
       | Some amphipod ->
         room_index = Amphipod.to_int amphipod
         && num_amphipods_solved t amphipod >= 4 - depth)
  ;;

  let is_solved t =
    num_amphipods_solved t A = 4
    && num_amphipods_solved t B = 4
    && num_amphipods_solved t C = 4
    && num_amphipods_solved t D = 4
  ;;

  let h t =
    let pos_is_amphipod_in_right_room pos =
      match amphipod_at_pos t pos with
      | None -> false
      | Some a ->
        Position_list.mem
          (Position_list.intersect
             (Position_list.room_for_amphipod a)
             (select_pos_list t a))
          pos
    in
    let for_amphipod a =
      let pos_list = select_pos_list t a in
      let num_solved = num_amphipods_solved t a in
      let first_room_pos =
        Position.of_spec (Room { room_index = Amphipod.to_int a; depth = 0 })
      in
      let room_penalty =
        match num_solved with
        | 0 -> 6
        | 1 -> 3
        | 2 -> 1
        | _ -> 0
      in
      let num_steps =
        Position_list.fold pos_list ~init:room_penalty ~f:(fun acc pos ->
          match is_pos_solved_amphipod t pos with
          | true -> acc
          | false ->
            (match pos_is_amphipod_in_right_room pos with
             | true -> acc + 1 (* this is overly conservative *)
             | false -> acc + Position.distance_if_no_blockers pos first_room_pos))
      in
      num_steps * Amphipod.energy_per_move a
    in
    for_amphipod A + for_amphipod B + for_amphipod C + for_amphipod D
  ;;

  let valid_moves_from_pos ({ a; b; c; d } as t) start_pos : Move_type.t =
    let rec aux res q =
      match Fqueue.dequeue q with
      | None -> res
      | Some (pos, q) ->
        let neighbors = Position_list.remove (Edges.neighbors pos) ~remove_these:res in
        let taken_spots =
          Position_list.union a b |> Position_list.union c |> Position_list.union d
        in
        let neighbors = Position_list.remove neighbors ~remove_these:taken_spots in
        let res = Position_list.union res neighbors in
        let q = Position_list.fold neighbors ~init:q ~f:Fqueue.enqueue in
        aux res q
    in
    let amphipod = amphipod_at_pos_exn t start_pos in
    let room_pos =
      Position_list.room_for_amphipod amphipod
      |> Position_list.fold
           ~init:(Some (Position.of_index 1))
           ~f:(fun acc pos ->
             let%bind.Option acc = acc in
             match amphipod_at_pos t pos with
             | None -> Some pos
             | Some amph ->
               (match [%equal: Amphipod.t] amphipod amph with
                | true -> Some acc
                | false -> None))
    in
    let all_accessible_positions =
      aux (Position_list.singleton start_pos) (Fqueue.singleton start_pos)
    in
    let can_make_it_to_good_pos =
      match room_pos with
      | None -> false
      | Some pos -> Position_list.mem all_accessible_positions pos
    in
    match can_make_it_to_good_pos with
    | true -> Forced (start_pos, Option.value_exn room_pos)
    | false ->
      (match Position.is_hallway start_pos with
       | true -> Choices []
       | false ->
         Choices
           (Position_list.intersect
              all_accessible_positions
              Position_list.hallways_not_in_front_of_room
            |> Position_list.to_list
            |> List.map ~f:(fun pos -> start_pos, pos)))
  ;;

  let valid_moves t =
    let res =
      fold_positions_with_amphipods
        t
        ~init:(Choices [] : Move_type.t)
        ~f:(fun acc pos ->
          match acc with
          | Forced _ -> acc
          | Choices acc ->
            (match is_pos_solved_amphipod t pos with
             | true -> Choices acc
             | false ->
               (match (valid_moves_from_pos t pos : Move_type.t) with
                | Forced _ as res -> res
                | Choices choices -> Choices (choices @ acc))))
    in
    match res with
    | Forced _ -> res
    | Choices [ (from, to_) ] -> Forced (from, to_)
    | Choices _ -> res
  ;;
end

module State = struct
  type t =
    { board : Board.t
    ; energy_spent : int
    ; h_val : int
    }
  [@@deriving sexp]

  let compare
        { board = b1; energy_spent = e1; h_val = h1 }
        { board = b2; energy_spent = e2; h_val = h2 }
    =
    let lb1 = e1 + h1 in
    let lb2 = e2 + h2 in
    match lb1 = lb2 with
    | true -> [%compare: int * Board.t] (e1, b1) (e2, b2)
    | false -> [%compare: int] lb1 lb2
  ;;

  let neighbor_states { board; energy_spent; h_val = _ } =
    let move_cost board ~from ~to_ =
      let amphipod = Board.amphipod_at_pos_exn board from in
      Position.distance_if_no_blockers from to_ * Amphipod.energy_per_move amphipod
    in
    let rec aux ?(first_iter = false) res = function
      | [] -> res
      | (board, energy_spent) :: tl ->
        let res = (board, energy_spent) :: res in
        (match (Board.valid_moves board : Move_type.t), first_iter with
         | Choices _, false -> aux res tl
         | Forced (from, to_), (true | false) ->
           aux
             res
             (( Board.apply_move board ~from ~to_
              , energy_spent + move_cost board ~from ~to_ )
              :: tl)
         | Choices choices, true ->
           let next_boards =
             List.map choices ~f:(fun (from, to_) ->
               ( Board.apply_move board ~from ~to_
               , energy_spent + move_cost board ~from ~to_ ))
           in
           aux res next_boards)
    in
    aux ~first_iter:true [] [ board, energy_spent ]
    |> List.sort_and_group ~compare:(Comparable.lift [%compare: Board.t] ~f:fst)
    |> List.map ~f:(fun group ->
      List.min_elt group ~compare:(Comparable.lift [%compare: int] ~f:snd)
      |> Option.value_exn)
    |> List.filter ~f:(fun (b, _energy_spent) -> not ([%equal: Board.t] b board))
    |> List.map ~f:(fun (board, energy_spent) ->
      { board; energy_spent; h_val = Board.h board })
  ;;
end

let solve start_board =
  let module Pq = Priority_queue.Make (State) in
  let rec aux pq visited =
    let ({ board; energy_spent; h_val = _ } as state : State.t), pq = Pq.get_exn pq in
    match Set.mem visited board with
    | true -> aux pq visited
    | false ->
      let visited = Set.add visited board in
      (match Board.is_solved board with
       | true -> energy_spent
       | false ->
         let pq =
           State.neighbor_states state
           |> List.fold
                ~init:pq
                ~f:(fun acc ({ board; energy_spent = _; h_val = _ } as state : State.t) ->
                  match Set.mem visited board with
                  | true -> acc
                  | false -> Pq.add acc state)
         in
         aux pq visited)
  in
  aux
    (Pq.singleton { board = start_board; energy_spent = 0; h_val = Board.h start_board })
    (Set.empty (module Board))
;;

let board1 = Board.make part1_rooms
let board2 = Board.make part2_rooms
let _test1 = Board.make test1_rooms
let _test2 = Board.make test2_rooms
let part1 (_ : string) = solve board1 |> Int.to_string
let part2 (_ : string) = solve board2 |> Int.to_string

let%expect_test "applying board move" =
  Board.print board1;
  Board.print board2;
  [%expect
    {|
    #############
    #...........#
    ###D#C#D#B###
      #B#A#A#C#
      #A#B#C#D#
      #A#B#C#D#
      #########
    #############
    #...........#
    ###D#C#D#B###
      #D#C#B#A#
      #D#B#A#C#
      #B#A#A#C#
      #########
    |}];
  let from = Position.of_spec (Room { room_index = 0; depth = 0 }) in
  let to_ = Position.of_spec (Hallway 2) in
  Board.apply_move board2 ~from ~to_ |> Board.print;
  [%expect
    {|
    #############
    #..D........#
    ###.#C#D#B###
      #D#C#B#A#
      #D#B#A#C#
      #B#A#A#C#
      #########
    |}];
  let from = Position.of_spec (Room { room_index = 1; depth = 0 }) in
  let to_ = Position.of_spec (Hallway 4) in
  Board.apply_move board2 ~from ~to_ |> Board.print;
  [%expect
    {|
    #############
    #....C......#
    ###D#.#D#B###
      #D#C#B#A#
      #D#B#A#C#
      #B#A#A#C#
      #########
    |}];
  let from = Position.of_spec (Room { room_index = 2; depth = 0 }) in
  let to_ = Position.of_spec (Hallway 6) in
  Board.apply_move board2 ~from ~to_ |> Board.print;
  [%expect
    {|
    #############
    #......D....#
    ###D#C#.#B###
      #D#C#B#A#
      #D#B#A#C#
      #B#A#A#C#
      #########
    |}];
  let from = Position.of_spec (Room { room_index = 3; depth = 0 }) in
  let to_ = Position.of_spec (Hallway 8) in
  Board.apply_move board2 ~from ~to_ |> Board.print;
  [%expect
    {|
    #############
    #........B..#
    ###D#C#D#.###
      #D#C#B#A#
      #D#B#A#C#
      #B#A#A#C#
      #########
    |}];
  let from = Position.of_spec (Room { room_index = 0; depth = 0 }) in
  let to_ = Position.of_spec (Hallway 10) in
  Board.apply_move board2 ~from ~to_ |> Board.print;
  [%expect
    {|
    #############
    #..........D#
    ###.#C#D#B###
      #D#C#B#A#
      #D#B#A#C#
      #B#A#A#C#
      #########
    |}]
;;

let%expect_test "all valid moves" =
  let moves = Board.valid_moves board2 in
  let moves =
    match moves with
    | Forced _ -> failwith "impossible"
    | Choices moves -> moves
  in
  List.iter moves ~f:(fun (from, to_) ->
    Board.apply_move board2 ~from ~to_ |> Board.print);
  [%expect
    {|
    #############
    #..........B#
    ###D#C#D#.###
      #D#C#B#A#
      #D#B#A#C#
      #B#A#A#C#
      #########
    #############
    #.........B.#
    ###D#C#D#.###
      #D#C#B#A#
      #D#B#A#C#
      #B#A#A#C#
      #########
    #############
    #.......B...#
    ###D#C#D#.###
      #D#C#B#A#
      #D#B#A#C#
      #B#A#A#C#
      #########
    #############
    #.....B.....#
    ###D#C#D#.###
      #D#C#B#A#
      #D#B#A#C#
      #B#A#A#C#
      #########
    #############
    #...B.......#
    ###D#C#D#.###
      #D#C#B#A#
      #D#B#A#C#
      #B#A#A#C#
      #########
    #############
    #.B.........#
    ###D#C#D#.###
      #D#C#B#A#
      #D#B#A#C#
      #B#A#A#C#
      #########
    #############
    #B..........#
    ###D#C#D#.###
      #D#C#B#A#
      #D#B#A#C#
      #B#A#A#C#
      #########
    #############
    #..........D#
    ###D#C#.#B###
      #D#C#B#A#
      #D#B#A#C#
      #B#A#A#C#
      #########
    #############
    #.........D.#
    ###D#C#.#B###
      #D#C#B#A#
      #D#B#A#C#
      #B#A#A#C#
      #########
    #############
    #.......D...#
    ###D#C#.#B###
      #D#C#B#A#
      #D#B#A#C#
      #B#A#A#C#
      #########
    #############
    #.....D.....#
    ###D#C#.#B###
      #D#C#B#A#
      #D#B#A#C#
      #B#A#A#C#
      #########
    #############
    #...D.......#
    ###D#C#.#B###
      #D#C#B#A#
      #D#B#A#C#
      #B#A#A#C#
      #########
    #############
    #.D.........#
    ###D#C#.#B###
      #D#C#B#A#
      #D#B#A#C#
      #B#A#A#C#
      #########
    #############
    #D..........#
    ###D#C#.#B###
      #D#C#B#A#
      #D#B#A#C#
      #B#A#A#C#
      #########
    #############
    #..........C#
    ###D#.#D#B###
      #D#C#B#A#
      #D#B#A#C#
      #B#A#A#C#
      #########
    #############
    #.........C.#
    ###D#.#D#B###
      #D#C#B#A#
      #D#B#A#C#
      #B#A#A#C#
      #########
    #############
    #.......C...#
    ###D#.#D#B###
      #D#C#B#A#
      #D#B#A#C#
      #B#A#A#C#
      #########
    #############
    #.....C.....#
    ###D#.#D#B###
      #D#C#B#A#
      #D#B#A#C#
      #B#A#A#C#
      #########
    #############
    #...C.......#
    ###D#.#D#B###
      #D#C#B#A#
      #D#B#A#C#
      #B#A#A#C#
      #########
    #############
    #.C.........#
    ###D#.#D#B###
      #D#C#B#A#
      #D#B#A#C#
      #B#A#A#C#
      #########
    #############
    #C..........#
    ###D#.#D#B###
      #D#C#B#A#
      #D#B#A#C#
      #B#A#A#C#
      #########
    #############
    #..........D#
    ###.#C#D#B###
      #D#C#B#A#
      #D#B#A#C#
      #B#A#A#C#
      #########
    #############
    #.........D.#
    ###.#C#D#B###
      #D#C#B#A#
      #D#B#A#C#
      #B#A#A#C#
      #########
    #############
    #.......D...#
    ###.#C#D#B###
      #D#C#B#A#
      #D#B#A#C#
      #B#A#A#C#
      #########
    #############
    #.....D.....#
    ###.#C#D#B###
      #D#C#B#A#
      #D#B#A#C#
      #B#A#A#C#
      #########
    #############
    #...D.......#
    ###.#C#D#B###
      #D#C#B#A#
      #D#B#A#C#
      #B#A#A#C#
      #########
    #############
    #.D.........#
    ###.#C#D#B###
      #D#C#B#A#
      #D#B#A#C#
      #B#A#A#C#
      #########
    #############
    #D..........#
    ###.#C#D#B###
      #D#C#B#A#
      #D#B#A#C#
      #B#A#A#C#
      #########
    |}]
;;
