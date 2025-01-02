open! Core

let do_stuff s fn john =
  let lines = String.split_lines s |> List.to_array in
  let n = String.length lines.(0) in
  let res = Array.make_matrix ~dimy:n ~dimx:26 0 in
  Array.iter lines ~f:(fun line ->
      String.iteri line ~f:(fun i c ->
          let n = Char.to_int c - Char.to_int 'a' in
          res.(n).(i) <- res.(n).(i) + 1));
  List.range 0 n
  |> List.map ~f:(fun char_num ->
         List.fold (List.range 0 26) ~init:('a', john)
           ~f:(fun (best_char, most_best) c ->
             if fn res.(c).(char_num) most_best then
               (Char.of_int_exn (c + Char.to_int 'a'), res.(c).(char_num))
             else (best_char, most_best))
         |> fst)
  |> String.of_list

let part1 s = do_stuff s (fun a b -> b < a) Int.min_value |> Ok

let part2 s =
  do_stuff s
    (fun a b -> if a = 0 then false else if b = 0 then true else b > a)
    Int.max_value
  |> Ok
