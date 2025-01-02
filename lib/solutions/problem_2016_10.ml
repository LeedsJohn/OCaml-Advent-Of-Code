open! Core

(* type bot = {  chips : int list; low_pass : int; high_pass : int }

module Bots = struct
    type t = bot Map.M(Int).t

    let add_chip t ~id ~chip =
        Map.update t id ~f:(function
            | None ->  {chips = [chip]; low_pass = Int.max_value; high_pass = Int.max_value}
            | Some ({chips; _} as bot) -> {bot with chips = chip :: chips})

    let set_passes t ~id ~low_pass ~high_pass =
        Map.update t id ~f:(function
            | None -> {low_pass; high_pass; chips = []}
            | Some bot -> {bot with low_pass; high_pass} )


   let of_string s = 
       String.split_lines s
       |> List.fold ~init:(Map.empty (module Int)) ~f:(fun acc line ->
               let words = String.split line ~on:' ' |> List.to_array in
               match words.(0) with
               | "value" -> add_chip acc ~id:(Int.of_string (words.(5)) ~chip:(Int.of_string words.(1))
               | _ -> (
                   let id = Int.of_string words.(1) in
                   let low_pass = Int.of_string words.(6) in
                   let high_pass = Int.of_string words.(
               )
       )
end *)

let part1 _ = Error (Error.of_string "Unimplemented")
let part2 _ = Error (Error.of_string "Unimplemented")
