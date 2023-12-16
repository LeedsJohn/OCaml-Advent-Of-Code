open! Core

let day = 16
let year = 2023

let parse_text text =
  String.split_lines text |> List.map ~f:String.to_array |> List.to_array

type direction = Right | Left | Up | Down [@@deriving compare, hash, sexp]

let get_direction_offsets = function
  | Right -> (1, 0)
  | Left -> (-1, 0)
  | Up -> (0, -1)
  | Down -> (0, 1)

module State = struct
  module T = struct
    type t = { x : int; y : int; dir : direction }
    [@@deriving compare, hash, sexp]
  end

  include T
  include Comparable.Make (T)
end

let get_next_states board (state : State.t) =
  (match board.(state.y).(state.x) with
  | '/' -> (
      match state.dir with
      | Right -> [ { state with y = state.y - 1; dir = Up } ]
      | Left -> [ { state with y = state.y + 1; dir = Down } ]
      | Up -> [ { state with x = state.x + 1; dir = Right } ]
      | Down -> [ { state with x = state.x - 1; dir = Left } ])
  | '\\' -> (
      match state.dir with
      | Right -> [ { state with y = state.y + 1; dir = Down } ]
      | Left -> [ { state with y = state.y - 1; dir = Up } ]
      | Up -> [ { state with x = state.x - 1; dir = Left } ]
      | Down -> [ { state with x = state.x + 1; dir = Right } ])
  | '-' -> (
      match state.dir with
      | Left | Right ->
          let dx, dy = get_direction_offsets state.dir in
          [ { state with x = state.x + dx; y = state.y + dy } ]
      | Up | Down ->
          [
            { state with x = state.x - 1; dir = Left };
            { state with x = state.x + 1; dir = Right };
          ])
  | '|' -> (
      match state.dir with
      | Up | Down ->
          let dx, dy = get_direction_offsets state.dir in
          [ { state with x = state.x + dx; y = state.y + dy } ]
      | Left | Right ->
          [
            { state with y = state.y - 1; dir = Up };
            { state with y = state.y + 1; dir = Down };
          ])
  | _ ->
      let dx, dy = get_direction_offsets state.dir in
      [ { state with x = state.x + dx; y = state.y + dy } ])
  |> List.filter ~f:(fun state ->
         state.x >= 0 && state.y >= 0
         && state.y < Array.length board
         && state.x < Array.length board.(0))

let get_num_energized board start_state =
  let visited = Hash_set.create (module State) in
  Hash_set.add visited start_state;
  let q = Queue.singleton start_state in
  while not (Queue.is_empty q) do
    let cur = Queue.dequeue_exn q in
    List.iter (get_next_states board cur) ~f:(fun s ->
        if not (Hash_set.mem visited s) then (
          Hash_set.add visited s;
          Queue.enqueue q s))
  done;
  Hash_set.fold visited ~init:[] ~f:(fun acc state ->
      if
        List.mem acc (state.x, state.y) ~equal:(fun (a, b) (c, d) ->
            a = c && b = d)
      then acc
      else (state.x, state.y) :: acc)
  |> List.length

let part1 fname =
  let board = parse_text (In_channel.read_all fname) in
  let start_state = { State.x = 0; y = 0; dir = Right } in
  get_num_energized board start_state |> Int.to_string

let part2 fname =
  let board = parse_text (In_channel.read_all fname) in
  let max_x, max_y = (Array.length board.(0), Array.length board) in
  let max_vert =
    List.fold (List.range 0 max_x) ~init:0 ~f:(fun acc x ->
        Int.max acc (get_num_energized board { State.x; y = 0; dir = Down })
        |> Int.max
             (get_num_energized board { State.x; y = max_y - 1; dir = Up }))
  in
  let max_hor =
    List.fold (List.range 0 max_y) ~init:0 ~f:(fun acc y ->
        Int.max acc (get_num_energized board { State.x = 0; y; dir = Right })
        |> Int.max
             (get_num_energized board { State.x = max_x - 1; y; dir = Left }))
  in
  Int.max max_vert max_hor |> Int.to_string
