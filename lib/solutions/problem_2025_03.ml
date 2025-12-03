open! Core

let parse stuff =
  String.split_lines stuff
  |> List.map ~f:(fun line ->
         String.to_array line
         |> Array.map ~f:(fun c -> Char.to_int c - Char.to_int '0'))

(* i think this could be done in linear time and i'd like to do that but i'm just trying
   to do something quick rn *)
let biggest line ~num_digits =
  let rec aux acc start num_left =
    if num_left = 0 then
      List.fold acc ~init:(0, 1) ~f:(fun (cur, mult) n ->
          (cur + (n * mult), mult * 10))
      |> fst
    else
      let next_val, next_i =
        List.range (start + 1) (Array.length line - num_left + 1)
        |> List.fold
             ~init:(line.(start), start)
             ~f:(fun (biggest, bi) i ->
               if line.(i) > biggest then (line.(i), i) else (biggest, bi))
      in
      aux (next_val :: acc) (next_i + 1) (num_left - 1)
  in
  aux [] 0 num_digits

let part1 stuff =
  parse stuff
  |> List.sum (module Int) ~f:(biggest ~num_digits:2)
  |> Int.to_string |> Ok

let part2 stuff =
  parse stuff
  |> List.sum (module Int) ~f:(biggest ~num_digits:12)
  |> Int.to_string |> Ok
