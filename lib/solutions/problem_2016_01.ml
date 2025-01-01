open! Core
open! Helpers

let part1 s =
  let a, b =
    String.split s ~on:' '
    |> List.map ~f:(fun s -> String.chop_suffix_if_exists s ~suffix:",")
    |> List.fold
         ~init:((0, 0), (0, -1))
         ~f:(fun (pos, dir) s ->
           let dir =
             if Char.equal (String.get s 0) 'R' then Coordinate.rotate_right dir
             else Coordinate.rotate_left dir
           in
           let pos =
             Coordinate.add
               (Coordinate.scale dir (Int.of_string (String.slice s 1 0)))
               pos
           in
           (pos, dir))
    |> fst
  in
  Int.abs a + Int.abs b |> Int.to_string |> Ok

let part2 s =
  let lines =
    String.split s ~on:' '
    |> List.map ~f:(fun s -> String.chop_suffix_if_exists s ~suffix:",")
  in
  let rec aux visited pos dir = function
    | [] -> Int.max_value
    | s :: tl -> (
        let dir =
          if Char.equal (String.get s 0) 'R' then Coordinate.rotate_right dir
          else Coordinate.rotate_left dir
        in
        let visited_positions =
          Coordinate.shoot_ray ~start:pos ~dir
            ~length:(1 + Int.of_string (String.slice s 1 0))
          |> List.tl_exn
        in
        match List.find visited_positions ~f:(Set.mem visited) with
        | Some (a, b) -> Int.abs a + Int.abs b
        | None ->
            let visited =
              Set.union visited
                (Set.of_list (module Coordinate) visited_positions)
            in
            aux visited (List.last_exn visited_positions) dir tl)
  in
  aux (Set.singleton (module Coordinate) (0, 0)) (0, 0) (0, -1) lines
  |> Int.to_string |> Ok
