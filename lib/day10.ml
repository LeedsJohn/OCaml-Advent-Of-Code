open Core

let day = 10
let year = 2023
let start_x = ref 0
let start_y = ref 0

let parse_text text =
  let lines = String.split_lines text in
  let lines = List.map lines ~f:(fun l -> "." ^ l ^ ".") in
  let first_line = String.make (List.hd_exn lines |> String.length) '.' in
  let lines = (first_line :: lines) @ [ first_line ] in
  List.map lines ~f:String.to_array |> Array.of_list

let get_next_pipes x y = function
  | '|' -> [ (x, y + 1); (x, y - 1) ]
  | '-' -> [ (x + 1, y); (x - 1, y) ]
  | 'L' -> [ (x + 1, y); (x, y - 1) ]
  | 'J' -> [ (x - 1, y); (x, y - 1) ]
  | '7' -> [ (x - 1, y); (x, y + 1) ]
  | 'F' -> [ (x + 1, y); (x, y + 1) ]
  | _ -> []

let get_neighbors board x y = get_next_pipes x y board.(y).(x)

let get_start_coords board =
  Array.find_mapi_exn board ~f:(fun y row ->
      match Array.findi row ~f:(fun _ c -> Char.equal c 'S') with
      | Some (x, _) ->
          start_x := x;
          start_y := y;
          Some (x, y)
      | None -> None)

let replace_start board =
  let get_pipe x y =
    List.find_exn (String.to_list "|-LJ7F") ~f:(fun pipe ->
        List.for_all (get_next_pipes x y pipe) ~f:(fun (nx, ny) ->
            if
              Int.min nx ny < 0
              || nx >= Array.length board.(0)
              || ny >= Array.length board
            then false
            else
              List.exists (get_neighbors board nx ny) ~f:(fun (mx, my) ->
                  x = mx && y = my)))
  in
  let x, y = get_start_coords board in
  board.(y).(x) <- get_pipe x y

module Position = struct
  module T = struct
    type t = int * int [@@deriving compare, hash, sexp_of]

    let make x y : t = (x, y)
    let get t : int * int = t
  end

  include T
  include Comparator.Make (T)
end

let part1 fname =
  let board = parse_text (In_channel.read_all fname) in
  let start_x, start_y = get_start_coords board in
  replace_start board;
  let q = Queue.create () in
  let visited = Hashtbl.create (module Position) in
  let start_coord = Position.make start_x start_y in
  Queue.enqueue q start_coord;
  Hashtbl.add_exn visited ~key:start_coord ~data:0;
  let res = ref 0 in
  while not (Queue.is_empty q) do
    let p = Position.get (Queue.dequeue_exn q) in
    let x, y = Position.get p in
    let d = Hashtbl.find_exn visited p in
    res := Int.max !res d;
    List.iter (get_neighbors board x y) ~f:(fun (nx, ny) ->
        let pos = Position.make nx ny in
        if Hashtbl.mem visited pos then ()
        else (
          Hashtbl.add_exn visited ~key:pos ~data:(d + 1);
          Queue.enqueue q pos))
  done;
  !res |> Int.to_string

let replace_useless_pipes board =
  let start_x, start_y = (!start_x, !start_y) in
  let q = Queue.create () in
  let visited = Hash_set.create (module Position) in
  let start_coord = Position.make start_x start_y in
  Queue.enqueue q start_coord;
  Hash_set.add visited start_coord;
  while not (Queue.is_empty q) do
    let p = Position.get (Queue.dequeue_exn q) in
    let x, y = Position.get p in
    List.iter (get_neighbors board x y) ~f:(fun (nx, ny) ->
        let pos = Position.make nx ny in
        if Hash_set.mem visited pos then ()
        else (
          Hash_set.add visited pos;
          Queue.enqueue q pos))
  done;
  Array.iteri board ~f:(fun y row ->
      Array.iteri row ~f:(fun x _ ->
          if not (Hash_set.mem visited (Position.make x y)) then
            board.(y).(x) <- '.'))

type cell = Wall | Outside | Unvisited

let is_unvisited = function Unvisited -> true | _ -> false

let expand_board board =
  let new_board =
    Array.make_matrix
      ~dimx:(Array.length board * 2)
      ~dimy:(Array.length board.(0) * 2)
      Unvisited
  in
  let set_in_between_walls x y =
    let neighbors = get_neighbors board x y in
    List.iter
      [ (1, 0); (-1, 0); (0, 1); (0, -1) ]
      ~f:(fun (dx, dy) ->
        if List.exists neighbors ~f:(fun (nx, ny) -> nx = x + dx && ny = y + dy)
        then new_board.((y * 2) + dy).((x * 2) + dx) <- Wall)
  in
  Array.iteri board ~f:(fun y row ->
      Array.iteri row ~f:(fun x _ ->
          if not (Char.equal board.(y).(x) '.') then
            new_board.(y * 2).(x * 2) <- Wall;
          set_in_between_walls x y));

  new_board

let flood_fill board =
  let get_neighbors x y =
    List.filter
      [ (x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1) ]
      ~f:(fun (x, y) ->
        Int.min x y >= 0
        && x < Array.length board.(0)
        && y < Array.length board
        && is_unvisited board.(y).(x))
  in
  let q = Queue.create () in
  Queue.enqueue q (0, 0);
  board.(0).(0) <- Outside;
  while not (Queue.is_empty q) do
    let x, y = Queue.dequeue_exn q in
    List.iter (get_neighbors x y) ~f:(fun (nx, ny) ->
        board.(ny).(nx) <- Outside;
        Queue.enqueue q (nx, ny))
  done

let part2 fname =
  let board = parse_text (In_channel.read_all fname) in
  replace_start board;
  replace_useless_pipes board;
  let board = expand_board board in
  flood_fill board;
  Array.foldi board ~init:0 ~f:(fun y acc row ->
      acc
      + Array.counti row ~f:(fun x cell ->
            y % 2 = 0 && x % 2 = 0 && is_unvisited cell))
  |> Int.to_string

let%expect_test "stuff" =
  let board =
    parse_text
      {|...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........|}
  in
  replace_start board;
  replace_useless_pipes board;
  let board = expand_board board in
  Array.iter board ~f:(fun row ->
      Array.iter row ~f:(fun cell ->
          let c = match cell with Wall -> '+' | _ -> '.' in
          print_string (String.of_char c));
      print_endline "");
  [%expect
    {|
    ..........................
    ..........................
    ..........................
    ..........................
    ....+++++++++++++++++.....
    ....+...............+.....
    ....+.+++++++++++++.+.....
    ....+.+...........+.+.....
    ....+.+...........+.+.....
    ....+.+...........+.+.....
    ....+.+...........+.+.....
    ....+.+...........+.+.....
    ....+.+++++...+++++.+.....
    ....+.....+...+.....+.....
    ....+.....+...+.....+.....
    ....+.....+...+.....+.....
    ....+++++++...+++++++.....
    ..........................
    ..........................
    ..........................
    ..........................
    .......................... |}];
  ();
  [%expect {| |}]
