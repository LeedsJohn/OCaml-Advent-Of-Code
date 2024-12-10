open! Core
open! Helpers

let of_string s = Board.of_string s |> Map.map ~f:Char.get_digit_exn

let get_trail_score board start =
  let visited = Hash_set.create (module Coordinate) in
  Hash_set.add visited start;
  let rec aux pos =
    let height = Map.find_exn board pos in
    if height = 9 then 1
    else
      Coordinate.neighbors pos
      |> Set.filter ~f:(fun pos ->
             match Map.find board pos with
             | None -> false
             | Some n -> n = height + 1 && not (Hash_set.mem visited pos))
      |> Set.sum
           (module Int)
           ~f:(fun pos ->
             Hash_set.add visited pos;
             aux pos)
  in
  match Map.find board start with Some 0 -> aux start | _ -> 0

(* if input was larger you'd have to cache by position i think *)
let get_rating board start =
  let rec aux pos =
    let height = Map.find_exn board pos in
    if height = 9 then 1
    else
      Coordinate.neighbors pos
      |> Set.filter ~f:(fun pos ->
             match Map.find board pos with
             | None -> false
             | Some n -> height + 1 = n)
      |> Set.sum (module Int) ~f:aux
  in
  match Map.find board start with Some 0 -> aux start | _ -> 0

let part1 s =
  let board = of_string s in
  Map.sumi
    (module Int)
    board
    ~f:(fun ~key:pos ~data:_ -> get_trail_score board pos)
  |> Int.to_string |> Ok

let part2 s =
  let board = of_string s in
  Map.sumi (module Int) board ~f:(fun ~key:pos ~data:_ -> get_rating board pos)
  |> Int.to_string |> Ok
