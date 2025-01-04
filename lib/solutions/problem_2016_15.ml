open! Core
open! Helpers

(* in reality i should just figure out how to solve the math equation but i made an
   unnecessary optimization of only checking times when the biggest disc is open *)

module Disc = struct
  type t = { id : int; num_positions : int; start_position : int }

  let of_string s =
    let ar = String.split s ~on:' ' |> List.to_array in
    let id = Int.of_string (String.drop_prefix ar.(1) 1) in
    let num_positions = Int.of_string ar.(3) in
    let start_position = Int.of_string (String.drop_suffix ar.(11) 1) in
    { id; num_positions; start_position }

  let is_open { num_positions; start_position; id } start_time =
    (start_position + start_time + id) % num_positions = 0
end

let get_discs s = String.split_lines s |> List.map ~f:Disc.of_string

let can_fall_through discs time =
  List.for_all discs ~f:(fun disc -> Disc.is_open disc time)

let first_start_time discs =
  let ({ num_positions; _ } as big_disc : Disc.t) =
    List.fold discs ~init:(List.hd_exn discs)
      ~f:(fun acc ({ num_positions; _ } as t : Disc.t) ->
        if num_positions > acc.num_positions then t else acc)
  in
  let jump = num_positions in
  let start_i =
    List.find_exn (List.range 0 num_positions) ~f:(fun i ->
        Disc.is_open big_disc i)
  in
  let i = ref start_i in
  while not (can_fall_through discs !i) do
    i := !i + jump
  done;
  !i

let part1 s =
  let discs = get_discs s in
  first_start_time discs |> Int.to_string |> Ok

let part2 s =
  let discs = get_discs s in
  let discs =
    discs
    @ [ { num_positions = 11; start_position = 0; id = List.length discs + 1 } ]
  in
  first_start_time discs |> Int.to_string |> Ok
