open! Core
open! Async

let get_module ~day ~year ~tag =
  let _, _, _, m =
    List.find_exn Solutions_list.all ~f:(fun (d, y, t, _) ->
        d = day && y = year && String.equal tag t)
  in
  m

let run ~day ~year ~tag ~(options : Options.t) =
  let s = get_module ~day ~year ~tag in
  let module S = (val s : Solution.T) in
  let test =
    match options.run_type with Test -> true | Real | Submit -> false
  in
  let%bind input = Input_handler.get_input ~day ~year ~test in
  let start_time = Time_float_unix.now () in
  let answer =
    match options.part with 1 -> S.part1 input | _ -> S.part2 input
  in
  let end_time = Time_float_unix.now () in
  print_endline
    ("Time: "
    ^ (Time_float_unix.diff end_time start_time
      |> Time_float_unix.Span.to_string_hum));
  print_s [%sexp (answer : string Or_error.t)];
  match (options.run_type, answer) with
  | Submit, Ok answer ->
      let%bind submission_result =
        Bridge.submit_solution ~day ~year ~part:options.part ~answer
      in
      print_s [%sexp (submission_result : Bridge.Submission_result.t)];
      return ()
  | _ -> return ()
