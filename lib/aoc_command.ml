open! Core
open! Async

let make_day =
  Command.async
    ~summary:"Initialize starter code for Advent of Code solution"
    (let%map_open.Command year = flag "-year" (required int) ~doc:" Year for solution"
     and day = flag "-day" (required int) ~doc:" Day for solution"
     and tag = flag "-tag" (optional string) ~doc:" Tag for solution" in
     fun () -> Make_day.make_day ~day ~year ~tag:(Option.value tag ~default:"") |> return)
;;

let run =
  Command.async
    ~summary:"Run Advent of Code solution"
    (let%map_open.Command year = flag "-year" (required int) ~doc:" Year for solution"
     and day = flag "-day" (required int) ~doc:" Day for solution"
     and part = flag "-part" (required int) ~doc:" Part 1 or part 2"
     and tag = flag "-tag" (optional string) ~doc:" Tag for solution"
     and run_type = anon ("run_type" %: Options.Run_type.arg) in
     fun () ->
       Runner.run
         ~day
         ~year
         ~tag:(Option.value tag ~default:"")
         ~options:{ run_type; part })
;;

let command =
  Command.group ~summary:"Advent of Code commands" [ "run", run; "make-day", make_day ]
;;
