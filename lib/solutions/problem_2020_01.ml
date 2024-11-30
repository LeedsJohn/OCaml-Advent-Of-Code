open! Core

let parse_input s = String.split_lines s |> List.map ~f:Int.of_string

let part1 s =
  let input = parse_input s in
  let combos = List.cartesian_product input input in
  Ok
    (List.find_map_exn combos ~f:(fun (n1, n2) ->
         if n1 + n2 = 2020 then Some (n1 * n2) else None)
    |> Int.to_string)

let part2 s =
  let input = parse_input s in
  let combos =
    List.cartesian_product input input
    |> List.cartesian_product input
    |> List.map ~f:(fun (n1, (n2, n3)) -> (n1, n2, n3))
  in
  Ok
    (List.find_map_exn combos ~f:(fun (n1, n2, n3) ->
         if n1 + n2 + n3 = 2020 then Some (n1 * n2 * n3) else None)
    |> Int.to_string)
