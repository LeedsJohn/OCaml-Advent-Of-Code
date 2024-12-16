open! Core
open! Helpers

module Garden = struct
  type t = { board : char Board.t; scale : int }

  [@@@warning "-32"]

  let of_string s scale =
    let s =
      String.split_lines s
      |> List.map ~f:(fun line ->
             String.to_list line
             |> List.map ~f:(fun c -> List.init scale ~f:(fun _ -> c))
             |> List.join |> String.of_list)
    in
    let s =
      List.fold s ~init:[] ~f:(fun acc line ->
          List.init scale ~f:(fun _ -> line) @ acc)
      |> List.rev |> String.concat_lines
    in
    let board = Board.of_string s in
    { board; scale }

  let to_string { board; _ } group =
    Map.mapi board ~f:(fun ~key:pos ~data:c ->
        if Set.mem group pos then '.' else c)
    |> Board.to_string

  let get { board; _ } (x, y) = Map.find board (x, y)

  let get_group ({ board; _ } as t) start_pos =
    let visited = Hash_set.create (module Coordinate) in
    Hash_set.add visited start_pos;
    let plant = Map.find_exn board start_pos in
    let rec aux pos =
      Set.iter (Coordinate.neighbors pos) ~f:(fun new_pos ->
          if
            (not (Hash_set.mem visited new_pos))
            && Char.equal (get t new_pos |> Option.value ~default:'.') plant
          then (
            Hash_set.add visited new_pos;
            aux new_pos))
    in
    aux start_pos;
    Set.of_hash_set (module Coordinate) visited

  let get_perimeter ({ board; scale } as t) start_pos =
    let group = get_group t start_pos in
    let plant = Map.find_exn board start_pos in
    Set.sum
      (module Int)
      group
      ~f:(fun pos ->
        Set.count (Coordinate.neighbors pos) ~f:(fun pos ->
            not (Char.equal (get t pos |> Option.value ~default:'.') plant)))
    / scale

  let get_area ({ scale; _ } as t) start_pos =
    (get_group t start_pos |> Set.length) / (scale * scale)

  let is_corner group pos =
    let out_corner =
      let neighbors =
        Set.filter (Coordinate.neighbors pos) ~f:(fun pos -> Set.mem group pos)
      in
      Set.mem group pos
      && Set.length neighbors = 2
      && not
           (Coordinate.equal
              (Set.fold neighbors ~init:(0, 0) ~f:Coordinate.add)
              (0, 0))
    in
    let in_corner =
      Set.count (Coordinate.neighbors8 pos) ~f:(fun pos -> Set.mem group pos)
      = 7
      && Set.mem group pos
    in
    out_corner || in_corner

  let count_corners group = Set.count group ~f:(fun pos -> is_corner group pos)
end

let part1 s =
  let ({ board; _ } as garden : Garden.t) = Garden.of_string s 1 in
  Map.fold board
    ~init:(Set.empty (module Coordinate), 0)
    ~f:(fun ~key:pos ~data:_ (visited, acc) ->
      match Set.mem visited pos with
      | true -> (visited, acc)
      | false ->
          let group = Garden.get_group garden pos in
          ( Set.union visited group,
            acc + (Garden.get_area garden pos * Garden.get_perimeter garden pos)
          ))
  |> snd |> Int.to_string |> Ok

let part2 s =
  let ({ board; _ } as garden : Garden.t) = Garden.of_string s 2 in
  Map.fold board
    ~init:(Set.empty (module Coordinate), 0)
    ~f:(fun ~key:pos ~data:_ (visited, acc) ->
      match Set.mem visited pos with
      | true -> (visited, acc)
      | false ->
          let group = Garden.get_group garden pos in
          ( Set.union visited group,
            acc + (Garden.get_area garden pos * Garden.count_corners group) ))
  |> snd |> Int.to_string |> Ok
