open! Core
open! Helpers

module Game = struct
  type t = { p1 : int * int; p2 : int * int; die : int; turns : int }
  [@@deriving sexp]

  let of_string s =
    let p1, s = Parse.take_int s in
    let p2, _ = Parse.take_int (String.drop_prefix s 1) in
    { p1 = (p1, 0); p2 = (p2, 0); die = 1; turns = 0 }

  let take_turn { p1 = p1_pos, p1_score; p2 = p2_pos, p2_score; die; turns } =
    let roll_die die =
      if die = 100 then (103, 3)
      else if die = 99 then (200, 2)
      else if die = 98 then (297, 1)
      else ((die * 3) + 3, die + 3)
    in
    let get_state pos score moves =
      let new_pos = ((pos + moves - 1) % 10) + 1 in
      (new_pos, score + new_pos)
    in
    let moves, die = roll_die die in
    match turns % 2 with
    | 0 ->
        {
          p1 = get_state p1_pos p1_score moves;
          p2 = (p2_pos, p2_score);
          die;
          turns = turns + 1;
        }
    | _ ->
        {
          p2 = get_state p2_pos p2_score moves;
          p1 = (p1_pos, p1_score);
          die;
          turns = turns + 1;
        }
end

module Stuff = struct
  type t = int * int * int * int * bool [@@deriving compare, sexp_of, hash]
end

let score_counts =
  List.cartesian_product (List.range 1 4) (List.range 1 4)
  |> List.cartesian_product (List.range 1 4)
  |> List.fold
       ~init:(Map.empty (module Int))
       ~f:(fun acc (n1, (n2, n3)) ->
         Map.update acc
           (n1 + n2 + n3)
           ~f:(function None -> 1 | Some n -> n + 1))

let memo = Hashtbl.create (module Stuff)

let rec stuff p1_pos p1_score p2_pos p2_score turn =
  let k = (p1_pos, p1_score, p2_pos, p2_score, turn) in
  if Hashtbl.mem memo k then Hashtbl.find_exn memo k
  else if p1_score >= 21 then (
    Hashtbl.add_exn memo ~key:k ~data:(1, 0);
    (1, 0))
  else if p2_score >= 21 then (
    Hashtbl.add_exn memo ~key:k ~data:(0, 1);
    (0, 1))
  else
    let update_pos pos moves = ((pos + moves - 1) % 10) + 1 in
    let get_new_stuff moves =
      match turn with
      | true ->
          let new_pos = update_pos p1_pos moves in
          (new_pos, p1_score + new_pos, p2_pos, p2_score)
      | false ->
          let new_pos = update_pos p2_pos moves in
          (p1_pos, p1_score, new_pos, p2_score + new_pos)
    in
    let res =
      Map.fold score_counts ~init:(0, 0) ~f:(fun ~key:moves ~data:count acc ->
          let p1_pos', p1_score', p2_pos', p2_score' = get_new_stuff moves in
          Coordinate.add acc
            (Coordinate.scale
               (stuff p1_pos' p1_score' p2_pos' p2_score' (not turn))
               count))
    in
    Hashtbl.add_exn memo ~key:k ~data:res;
    res

let part1 s =
  let rec aux ({ p1 = _, p1_score; p2 = _, p2_score; turns; _ } as t : Game.t) =
    if p1_score >= 1000 then p2_score * turns * 3
    else if p2_score >= 1000 then p1_score * turns * 3
    else aux (Game.take_turn t)
  in
  let game = Game.of_string s in
  aux game |> Int.to_string |> Ok

let part2 s =
  let ({ p1 = p1_pos, _; p2 = p2_pos, _; _ } : Game.t) = Game.of_string s in
  let x, y = stuff p1_pos 0 p2_pos 0 true in
  Int.max x y |> Int.to_string |> Ok
