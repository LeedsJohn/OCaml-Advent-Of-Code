open! Core

let parse s =
  String.split s ~on:','
  |> List.foldi
       ~init:(Map.empty (module Int))
       ~f:(fun i acc num -> Map.add_exn acc ~key:i ~data:(Int.of_string num))
;;

let rec solve intcode i =
  let get_three i =
    ( Map.find_exn intcode (i + 1)
    , Map.find_exn intcode (i + 2)
    , Map.find_exn intcode (i + 3) )
  in
  let apply pos1 pos2 store fn =
    let res = fn (Map.find_exn intcode pos1) (Map.find_exn intcode pos2) in
    Map.set intcode ~key:store ~data:res
  in
  match Map.find_exn intcode i with
  | 99 -> Some intcode
  | 1 ->
    let a, b, c = get_three i in
    let intcode = apply a b c Int.( + ) in
    solve intcode (i + 4)
  | 2 ->
    let a, b, c = get_three i in
    let intcode = apply a b c Int.( * ) in
    solve intcode (i + 4)
  | _ -> None
;;

let rec solve2 intcode x y goal =
  let new_intcode = Map.set intcode ~key:1 ~data:x |> Map.set ~key:2 ~data:y in
  let res = solve new_intcode 0 in
  let nx, ny = if y = x then x + 1, 0 else x, y + 1 in
  match res with
  | Some ic -> if Map.find_exn ic 0 = goal then x, y else solve2 intcode nx ny goal
  | _ -> solve2 intcode nx ny goal
;;

let part1 s =
  let intcode = parse s in
  let intcode = Map.set intcode ~key:1 ~data:12 in
  let intcode = Map.set intcode ~key:2 ~data:2 in
  let intcode = solve intcode 0 |> Option.value_exn in
  Map.find_exn intcode 0 |> Int.to_string
;;

let part2 s =
  let intcode = parse s in
  let x, y = solve2 intcode 0 0 19690720 in
  print_s [%sexp ((x, y) : int * int)];
  Int.to_string ((100 * x) + y)
;;
