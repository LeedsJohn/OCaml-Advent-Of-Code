open! Core

module Board = struct
  type t =
    { board : bool Helpers.Board.t
    ; max_x : int
    }

  let get { board; max_x } (x, y) = Map.find_exn board (x % (max_x + 1), y)

  let of_string s =
    let board = Helpers.Board.of_string s in
    let board = Map.map board ~f:(Char.equal '#') in
    let max_x = Helpers.Board.max_coordinates board |> fst in
    { board; max_x }
  ;;
end

let traverse (board : Board.t) ~dx ~dy =
  let max_y = Helpers.Board.max_coordinates board.board |> snd in
  let rec aux (x, y) acc =
    if y > max_y
    then acc
    else aux (x + dx, y + dy) (acc + Bool.to_int (Board.get board (x, y)))
  in
  aux (0, 0) 0
;;

let solve1 board = traverse board ~dx:3 ~dy:1

let solve2 board =
  List.map [ 1, 1; 3, 1; 5, 1; 7, 1; 1, 2 ] ~f:(fun (dx, dy) -> traverse board ~dx ~dy)
  |> List.fold ~init:1 ~f:(fun acc n -> acc * n)
;;

let part1 s =
  let board = Board.of_string s in
  solve1 board |> Int.to_string
;;

let part2 s =
  let board = Board.of_string s in
  solve2 board |> Int.to_string
;;
