open! Core
open! Helpers

module City = struct
  type t = {
    board : char Board.t;
    antennas : Set.M(Coordinate).t Map.M(Char).t;
  }

  let is_in_range { board; _ } pos = Map.mem board pos

  let of_string s =
    let board = Board.of_string s in
    let antennas =
      Map.fold board
        ~init:(Map.empty (module Char))
        ~f:(fun ~key:pos ~data:c acc ->
          if Char.(c = '.') then acc
          else
            Map.update acc c ~f:(function
              | None -> Set.singleton (module Coordinate) pos
              | Some s -> Set.add s pos))
    in
    { board; antennas }

  let is_spot_antinode { antennas; _ } pos =
    Map.exists antennas ~f:(fun s ->
        Set.exists s ~f:(fun antenna ->
            let offset = Coordinate.sub antenna pos in
            (not (Coordinate.equal offset (0, 0)))
            && Set.mem s (Coordinate.add antenna offset)))

  let is_spot_antinode2 ({ antennas; _ } as t) pos =
    Map.exists antennas ~f:(fun s ->
        Set.exists s ~f:(fun antenna ->
            let offset = Coordinate.sub antenna pos |> Coordinate.scale_down in
            let rec aux pos =
              if not (is_in_range t pos) then false
              else if (not (Coordinate.equal antenna pos)) && Set.mem s pos then
                true
              else aux (Coordinate.add pos offset)
            in
            if Coordinate.equal offset (0, 0) then false else aux pos))
end

let part1 s =
  let t = City.of_string s in
  List.count (Map.keys t.board) ~f:(City.is_spot_antinode t)
  |> Int.to_string |> Ok

let part2 s =
  let t = City.of_string s in
  List.count (Map.keys t.board) ~f:(City.is_spot_antinode2 t)
  |> Int.to_string |> Ok
