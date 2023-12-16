open Core

let day = 4
let year = 2023

let parse_input fname =
  In_channel.read_lines fname
  |> List.map ~f:(fun line ->
         let card =
           String.split_on_chars line ~on:[ ':'; '|' ]
           |> List.tl_exn
           |> List.map ~f:(fun nums ->
                  String.split nums ~on:' '
                  |> List.filter ~f:(fun n -> not (String.is_empty n))
                  |> List.map ~f:Int.of_string)
           |> Array.of_list
         in
         (* print_s [%sexp (card : int list array)]; *)
         (Set.of_list (module Int) card.(0), card.(1)))

let part1 fname =
  parse_input fname
  |> List.fold ~init:0 ~f:(fun acc (winners, nums) ->
         let num_winners = List.count nums ~f:(fun n -> Set.mem winners n) in
         if num_winners = 0 then acc else acc + Int.pow 2 (num_winners - 1))
  |> Int.to_string

(* I'm surprised this works because I think it could go out of bounds *)
let part2 fname =
  let cards = parse_input fname in
  let copies = Array.create ~len:(List.length cards) 1 in
  List.iteri cards ~f:(fun i (winners, nums) ->
      let num_winners = List.count nums ~f:(fun n -> Set.mem winners n) in
      List.iter
        (List.range (i + 1) (i + num_winners + 1))
        ~f:(fun j -> copies.(j) <- copies.(j) + copies.(i)));
  Array.fold copies ~init:0 ~f:( + ) |> Int.to_string
