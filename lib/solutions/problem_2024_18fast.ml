open! Core
open! Helpers

let dim = 70

let of_string s =
  let board = Array.make_matrix ~dimx:(dim + 1) ~dimy:(dim + 1) (-1) in
  let coords =
    String.split_lines s
    |> List.map ~f:(fun line ->
      let x, line = Parse.take_next_int line in
      let y, _ = Parse.take_next_int line in
      x, y)
    |> List.to_array
  in
  Array.iteri coords ~f:(fun i (x, y) -> board.(y).(x) <- i);
  board, coords
;;

let bfs board max_wall =
  let q = Queue.singleton ((0, 0), 0) in
  let res = ref 0 in
  while !res = 0 do
    let pos, steps = Queue.dequeue_exn q in
    List.iter (Coordinate.neighbors pos) ~f:(fun (x, y) ->
      if
        Int.min x y >= 0
        && Int.max x y <= dim
        && (board.(y).(x) = -1 || board.(y).(x) > max_wall)
      then (
        board.(y).(x) <- -2;
        if Coordinate.equal (x, y) (dim, dim) then res := steps + 1;
        Queue.enqueue q ((x, y), steps + 1)))
  done;
  !res
;;

let mark_reachable board ((x, y) as start_pos) =
  let q = Queue.singleton start_pos in
  board.(y).(x) <- -2;
  while not (Queue.is_empty q) do
    let pos = Queue.dequeue_exn q in
    List.iter (Coordinate.neighbors pos) ~f:(fun (x, y) ->
      if Int.min x y >= 0 && Int.max x y <= dim && board.(y).(x) = -1
      then (
        board.(y).(x) <- -2;
        Queue.enqueue q (x, y)))
  done
;;

let part1 s =
  let board, _ = of_string s in
  bfs board 1024 |> Int.to_string
;;

let part2 s =
  let board, coords = of_string s in
  let i = ref (Array.length coords - 1) in
  mark_reachable board (0, 0);
  while board.(dim).(dim) <> -2 do
    let x, y = coords.(!i) in
    board.(y).(x) <- -1;
    if
      List.exists
        (Coordinate.neighbors (x, y))
        ~f:(fun (x, y) -> Int.min x y >= 0 && Int.max x y <= dim && board.(y).(x) = -2)
    then mark_reachable board (x, y);
    i := !i - 1
  done;
  let x, y = coords.(!i + 1) in
  [%string "%{Int.to_string x},%{Int.to_string y}"]
;;
