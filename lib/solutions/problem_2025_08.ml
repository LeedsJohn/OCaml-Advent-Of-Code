(* this would be an interesting one to write faster *)
open! Core
open Helpers

module Coord3 = struct
  type t = int * int * int

  let dist (x1, y1, z1) (x2, y2, z2) =
    let x1, y1, z1, x2, y2, z2 =
      ( Float.of_int x1
      , Float.of_int y1
      , Float.of_int z1
      , Float.of_int x2
      , Float.of_int y2
      , Float.of_int z2 )
    in
    Float.sqrt
      (Float.square (x2 -. x1) +. Float.square (y2 -. y1) +. Float.square (z2 -. z1))
  ;;
end

module Beacon = struct
  type t =
    { pos : Coord3.t
    ; group : int Union_find.t
    }
  [@@deriving fields ~getters]
end

(* oops this was not the problem *)
let rec _union_closest_unconnected = function
  | ({ Beacon.group = g1; pos = _ }, { Beacon.group = g2; pos = _ }, _) :: tl ->
    if Union_find.same_class g1 g2
    then _union_closest_unconnected tl
    else (
      Union_find.union g1 g2;
      tl)
  | _ -> raise_s [%message "bruh 3"]
;;

let parse s =
  String.split_lines s
  |> List.map ~f:Parse.line_numbers
  |> List.map ~f:(function
    | [ x; y; z ] -> x, y, z
    | _ -> raise_s [%message "bruh"])
  |> List.mapi ~f:(fun i pos -> { Beacon.pos; group = Union_find.create i })
;;

let group_sizes beacons =
  Array.map beacons ~f:Beacon.group
  |> Array.map ~f:Union_find.get
  |> Array.fold
       ~init:(Map.empty (module Int))
       ~f:(fun acc n ->
         Map.update acc n ~f:(function
           | None -> 1
           | Some n -> 1 + n))
  |> Map.to_alist
  |> List.map ~f:snd
  |> List.sort ~compare:Int.compare
  |> List.rev
;;

let part1 s =
  let n = 1000 in
  let beacons = parse s |> Array.of_list in
  List.cartesian_product
    (List.range 0 (Array.length beacons))
    (List.range 0 (Array.length beacons))
  |> List.filter ~f:(fun (i, j) -> i < j)
  |> List.sort
       ~compare:
         (Comparable.lift Float.compare ~f:(fun (i1, i2) ->
            let { Beacon.pos = p1; group = _ } = beacons.(i1) in
            let { Beacon.pos = p2; group = _ } = beacons.(i2) in
            Coord3.dist p1 p2))
  |> List.iteri ~f:(fun i (j, k) ->
    if i < n then Union_find.union beacons.(j).group beacons.(k).group);
  match group_sizes beacons with
  | a :: b :: c :: _ -> Int.to_string (a * b * c)
  | _ -> raise_s [%message "bruh2"]
;;

let part2 s =
  let beacons = parse s |> Array.of_list in
  let bruh = ref false in
  let res = ref 0 in
  let all_connected () =
    if !bruh
    then true
    else (
      let res = List.length (group_sizes beacons) = 1 in
      bruh := res;
      res)
  in
  List.cartesian_product
    (List.range 0 (Array.length beacons))
    (List.range 0 (Array.length beacons))
  |> List.filter ~f:(fun (i, j) -> i < j)
  |> List.sort
       ~compare:
         (Comparable.lift Float.compare ~f:(fun (i1, i2) ->
            let { Beacon.pos = p1; group = _ } = beacons.(i1) in
            let { Beacon.pos = p2; group = _ } = beacons.(i2) in
            Coord3.dist p1 p2))
  |> List.iter ~f:(fun (i, j) ->
    if not (all_connected ())
    then (
      Union_find.union beacons.(i).group beacons.(j).group;
      res := fst3 beacons.(i).pos * fst3 beacons.(j).pos));
  Int.to_string !res
;;
