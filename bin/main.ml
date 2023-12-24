open Core
open Aoc

(* TODO: how to do this without manually updating this list *)
let days =
  [
    (module Day01 : Problem.Problem);
    (module Day02 : Problem.Problem);
    (module Day03 : Problem.Problem);
    (module Day04 : Problem.Problem);
    (module Day05 : Problem.Problem);
    (module Day06 : Problem.Problem);
    (module Day07 : Problem.Problem);
    (module Day08 : Problem.Problem);
    (module Day09 : Problem.Problem);
    (module Day10 : Problem.Problem);
    (module Day11 : Problem.Problem);
    (module Day12 : Problem.Problem);
    (module Day13 : Problem.Problem);
    (module Day14 : Problem.Problem);
    (module Day15 : Problem.Problem);
    (module Day16 : Problem.Problem);
    (module Day17 : Problem.Problem);
    (module Day18 : Problem.Problem);
    (module Day19 : Problem.Problem);
    (module Day20 : Problem.Problem);
    (module Day21 : Problem.Problem);
    (module Day22 : Problem.Problem);
    (module Day23 : Problem.Problem);
    (module Day24 : Problem.Problem);
  ]

let get_module day year =
  List.find_exn days ~f:(fun m ->
      let module P = (val m : Problem.Problem) in
      P.day = day && P.year = year)

let run day year what_to_run =
  let day_s = if day < 10 then "0" ^ Int.to_string day else Int.to_string day in
  let test_input = "test_input/" ^ day_s ^ ".in" in
  let real_input = "input/" ^ day_s ^ ".in" in
  let p = get_module day year in
  let module P = (val p : Problem.Problem) in
  (match what_to_run with
  | `Test | `Test1 | `All | `All1 ->
      print_endline ("Part 1 (test): " ^ P.part1 test_input)
  | _ -> ());
  (match what_to_run with
  | `Real | `Real1 | `All | `All1 ->
      print_endline ("Part 1: " ^ P.part1 real_input)
  | _ -> ());
  (match what_to_run with
  | `Test | `Test2 | `All | `All2 ->
      print_endline ("Part 2 (test): " ^ P.part2 test_input)
  | _ -> ());
  (match what_to_run with
  | `Real | `Real2 | `All | `All2 ->
      print_endline ("Part 2: " ^ P.part2 real_input)
  | _ -> ());
  ()

let what_to_run =
  Command.Arg_type.create (fun s ->
      match s with
      | "test" -> `Test
      | "test1" -> `Test1
      | "test2" -> `Test2
      | "real" -> `Real
      | "real1" -> `Real1
      | "real2" -> `Real2
      | "all" -> `All
      | "all1" -> `All1
      | "all2" -> `All2
      | _ -> failwith "Invalid run choice")

let command =
  Command.basic ~summary:"Run advent of code solution"
    ~readme:(fun () ->
      "Day, year, what_to_run (test, real, or both), part (1, 2, or both)")
    (let%map_open.Command day = anon ("day" %: int)
     and year = anon ("year" %: int)
     and what_to_run =
       anon (maybe_with_default `All ("what_to_run" %: what_to_run))
     in
     fun () -> run day year what_to_run)

let _ = Command_unix.run command
