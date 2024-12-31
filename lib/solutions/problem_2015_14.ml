open! Core

(* Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds. *)

module Reindeer = struct
  type t = { name : string; fly_speed : int; fly_time : int; rest_time : int }

  let of_string s =
    String.split_lines s
    |> List.map ~f:(fun line ->
           let line = String.split line ~on:' ' |> List.to_array in
           let name = line.(0) in
           let fly_speed = Int.of_string line.(3) in
           let fly_time = Int.of_string line.(6) in
           let rest_time = Int.of_string line.(13) in
           { name; fly_speed; fly_time; rest_time })

  let distance_covered_in { fly_speed; fly_time; rest_time; _ } time =
    let num_cycles = time / (fly_time + rest_time) in
    let remaining_time = time - (num_cycles * (fly_time + rest_time)) in
    let extra_distance = fly_speed * Int.min remaining_time fly_time in
    let initial_distance = num_cycles * fly_time * fly_speed in
    initial_distance + extra_distance

  let contest ts end_time =
    let rec aux acc time =
      if time = end_time then acc
      else
        let best_reindeer, _ =
          List.fold ts ~init:([ "" ], -1) ~f:(fun (best_r, best_d) r ->
              let d = distance_covered_in r time in
              if d = best_d then (r.name :: best_r, best_d)
              else if d > best_d then ([ r.name ], d)
              else (best_r, best_d))
        in
        let acc =
          List.fold best_reindeer ~init:acc ~f:(fun acc r ->
              Map.update acc r ~f:(function None -> 1 | Some n -> n + 1))
        in
        aux acc (time + 1)
    in
    aux (Map.empty (module String)) 1
    |> Map.data
    |> List.max_elt ~compare:Int.compare
    |> Option.value_exn
end

let part1 s =
  Reindeer.of_string s
  |> List.map ~f:(fun t -> Reindeer.distance_covered_in t 2503)
  |> List.max_elt ~compare:Int.compare
  |> Option.value_exn |> Int.to_string |> Ok

let part2 s =
  let reindeer = Reindeer.of_string s in
  Reindeer.contest reindeer 2503 |> Int.to_string |> Ok
