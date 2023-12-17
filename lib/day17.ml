open Core

let day = 17
let year = 2023

let parse_text text =
  String.strip text |> String.split_lines
  |> List.map ~f:(fun line ->
         Array.map (String.to_array line) ~f:Char.get_digit_exn)
  |> List.to_array

type direction = Up | Right | Down | Left [@@deriving compare, hash, sexp]

module State = struct
  type t = { x : int; y : int; dir : direction; line_length : int; score : int }
  [@@deriving sexp]

  let compare t1 t2 =
    if t1.line_length <> t2.line_length then
      Int.compare t1.line_length t2.line_length
    else if t1.x <> t2.x then Int.compare t1.x t2.x
    else if t1.y <> t2.y then Int.compare t1.y t2.y
    else compare_direction t1.dir t2.dir

  let hash t =
    t.x + (t.y * 2011) + (t.line_length * 7919)
    + ((compare_direction Up t.dir + 10) * 1000009)
end

let adjacent_directions = function
  | Up | Down -> [ Left; Right ]
  | Left | Right -> [ Up; Down ]

let direction_offsets = function
  | Up -> (0, -1)
  | Down -> (0, 1)
  | Left -> (-1, 0)
  | Right -> (1, 0)

let same_direction dir1 dir2 = compare_direction dir1 dir2 = 0

let advance_position board (state : State.t) dir min_length max_length =
  let min_length = if same_direction state.dir dir then 1 else min_length in
  let new_line_length =
    if same_direction state.dir dir then state.line_length + 1 else min_length
  in
  let dx, dy = direction_offsets dir in
  let x, y = (state.x + (dx * min_length), state.y + (dy * min_length)) in
  if
    new_line_length > max_length
    || x < 0 || y < 0
    || x >= Array.length board.(0)
    || y >= Array.length board
  then None
  else
    let score =
      List.fold
        (List.range 1 (min_length + 1))
        ~init:state.score
        ~f:(fun acc i -> acc + board.(state.y + (i * dy)).(state.x + (i * dx)))
    in
    Some { State.x; y; dir; line_length = new_line_length; score }

let get_next_positions board state min_length max_length =
  let next_states =
    advance_position board state state.dir min_length max_length
    :: List.map (adjacent_directions state.dir) ~f:(fun d ->
           advance_position board state d min_length max_length)
  in
  List.filter_map next_states ~f:Fn.id

let minimize_loss board min_length max_length =
  let max_x = Array.length board.(0) in
  let max_y = Array.length board in
  let pq =
    Pairing_heap.create
      ~cmp:(fun (state1 : State.t) (state2 : State.t) ->
        if state1.score <> state2.score then state1.score - state2.score
        else
          max_x - state1.x + max_y - state1.y
          - (max_x - state2.x + max_y - state2.y))
      ()
  in
  let visited = Hash_set.create (module State) in
  let start_state =
    { State.x = 0; y = 0; dir = Right; line_length = 0; score = 0 }
  in
  Pairing_heap.add pq start_state;
  let res = ref None in
  while Option.is_none !res do
    let cur_state = Pairing_heap.pop_exn pq in
    if Hash_set.mem visited cur_state then ()
    else (
      Hash_set.add visited cur_state;
      if cur_state.x + 1 = max_x && cur_state.y + 1 = max_y then
        res := Some cur_state.score
      else
        List.iter (get_next_positions board cur_state min_length max_length)
          ~f:(fun s ->
            if not (Hash_set.mem visited s) then Pairing_heap.add pq s))
  done;
  Option.value_exn !res

let part1 fname =
  let board = parse_text (In_channel.read_all fname) in
  minimize_loss board 1 3 |> Int.to_string

let part2 fname =
  let board = parse_text (In_channel.read_all fname) in
  minimize_loss board 4 10 |> Int.to_string
