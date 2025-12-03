open! Core
open! Helpers

module Game = struct
  type t =
    { board : bool Board.t
    ; warden : Coordinate.t * Coordinate.t
    }

  let of_string s =
    let b = Board.of_string s in
    let board = Map.map b ~f:(fun c -> Char.equal c '#') in
    let warden_pos =
      Map.to_alist b |> List.find_exn ~f:(fun (_, c) -> Char.equal c '^') |> fst
    in
    { board; warden = warden_pos, (0, -1) }
  ;;

  let _to_string { board; warden = pos, _ } =
    let b =
      Map.map board ~f:(fun c -> if c then '#' else '.') |> Map.set ~key:pos ~data:'^'
    in
    Board.to_string b
  ;;

  let step { board; warden = pos, dir } =
    match Map.find board (Coordinate.add pos dir) with
    | Some false -> Some { board; warden = Coordinate.add pos dir, dir }
    | Some true -> Some { board; warden = pos, Coordinate.rotate_right dir }
    | None -> None
  ;;

  module Pos_and_dir = struct
    type t = Coordinate.t * Coordinate.t [@@deriving compare, sexp, hash]
  end

  let does_loop t =
    let visited = Hash_set.create (module Pos_and_dir) in
    let rec aux ({ warden = pos, dir; _ } as t) =
      if Hash_set.mem visited (pos, dir)
      then true
      else (
        Hash_set.add visited (pos, dir);
        match step t with
        | None -> false
        | Some t -> aux t)
    in
    aux t
  ;;
end

let part1 s =
  let game = Game.of_string s in
  let visited = Hash_set.create (module Coordinate) in
  let rec aux ({ warden = pos, _; _ } as t : Game.t) =
    Hash_set.add visited pos;
    match Game.step t with
    | None -> ()
    | Some t -> aux t
  in
  aux game;
  Hash_set.length visited |> Int.to_string
;;

let part2 s =
  let game = Game.of_string s in
  let spots_to_make_wall =
    Map.to_alist game.board
    |> List.filter ~f:(fun (pos, v) ->
      (not v) && not (Coordinate.equal pos (fst game.warden)))
    |> List.map ~f:fst
  in
  List.count spots_to_make_wall ~f:(fun spot ->
    let game = { game with board = Map.set game.board ~key:spot ~data:true } in
    Game.does_loop game)
  |> Int.to_string
;;
