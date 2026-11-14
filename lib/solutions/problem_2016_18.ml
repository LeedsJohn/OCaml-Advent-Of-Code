open! Core
open! Helpers

(* for part 2 i should only keep the most recent row but it still runs in ~35 seconds
   on my macbook air so whatever *)

module Room = struct
  type t =
    { board : bool Hashtbl.M(Coordinate).t
    ; length : int
    }

  let of_string s =
    let board = Hashtbl.create (module Coordinate) in
    String.iteri s ~f:(fun x c ->
      let data = Char.equal c '.' in
      Hashtbl.add_exn board ~key:(Coordinate.make ~x ~y:0) ~data);
    let length = String.length s in
    { board; length }
  ;;

  let rec is_safe ({ board; length } as t) coord =
    let x, y = Coordinate.xy coord in
    if x < 0 || x >= length || y < 0
    then true
    else (
      match Hashtbl.find board (Coordinate.make ~x ~y) with
      | Some res -> res
      | None ->
        let a, b, c =
          ( not (is_safe t (Coordinate.make ~x:(x - 1) ~y:(y - 1)))
          , not (is_safe t (Coordinate.make ~x ~y:(y - 1)))
          , not (is_safe t (Coordinate.make ~x:(x + 1) ~y:(y - 1))) )
        in
        let res =
          not
            ((a && b && not c)
             || (b && c && not a)
             || (a && (not b) && not c)
             || (c && (not a) && not b))
        in
        Hashtbl.add_exn board ~key:(Coordinate.make ~x ~y) ~data:res;
        res)
  ;;

  let count_safe_tiles ({ length; _ } as t) rows =
    List.sum
      (module Int)
      (List.range 0 length)
      ~f:(fun x ->
        List.count (List.range 0 rows) ~f:(fun y -> is_safe t (Coordinate.make ~x ~y)))
  ;;

  let _print ({ length; _ } as t) rows =
    List.iter (List.range 0 rows) ~f:(fun y ->
      List.range 0 length
      |> List.map ~f:(fun x -> if is_safe t (Coordinate.make ~x ~y) then '.' else '^')
      |> String.of_list
      |> print_endline)
  ;;
end

let part1 s =
  let stuff = Room.of_string s in
  Room.count_safe_tiles stuff 40 |> Int.to_string
;;

let part2 s =
  let stuff = Room.of_string s in
  Room.count_safe_tiles stuff 400000 |> Int.to_string
;;
