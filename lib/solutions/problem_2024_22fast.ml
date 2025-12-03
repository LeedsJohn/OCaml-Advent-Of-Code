open! Core

let mod_num = 16777216

let step n =
  let do_f f n = Int.( % ) (Int.bit_xor (f n) n) mod_num in
  do_f (fun n -> n * 64) n |> do_f (fun n -> n / 32) |> do_f (fun n -> n * 2048)
;;

let step_times n times = List.fold (List.range 0 times) ~init:n ~f:(fun acc _ -> step acc)

module Changes = struct
  type t = int [@@deriving compare, equal, of_sexp, hash]

  let add_new_dif t d =
    let d = d + 9 in
    (t % Int.pow 20 3 * 20) + d
  ;;

  let num_combos = Int.pow 20 4

  let sexp_of_t t =
    let rec aux acc n =
      if List.length acc = 4 then acc else aux (((n % 20) - 9) :: acc) (n / 20)
    in
    [%sexp (aux [] t : int list)]
  ;;
end

let generate_changes ?(steps = 2000) n =
  let prices =
    n
    :: (List.fold_map
          (List.range 0 (steps - 1))
          ~init:n
          ~f:(fun acc _ ->
            let res = step acc in
            res, res)
        |> snd)
  in
  let prices = List.map prices ~f:(fun n -> Int.( % ) n 10) in
  let changes =
    match prices with
    | a :: b :: c :: d :: e :: tl ->
      let initial_changes =
        List.foldi
          [ e - d; d - c; c - b; b - a ]
          ~init:0
          ~f:(fun i acc n -> acc + ((n + 9) * Int.pow 20 i))
      in
      (initial_changes, e)
      :: (List.fold_map tl ~init:(initial_changes, e) ~f:(fun (changes, prev_num) price ->
            let changes = Changes.add_new_dif changes (price - prev_num) in
            (changes, price), (changes, price))
          |> snd)
    | prices -> raise_s [%message "not enough prices" (prices : int list)]
  in
  let res = Hashtbl.create (module Changes) in
  List.iter changes ~f:(fun (changes, p) ->
    Hashtbl.update res changes ~f:(function
      | None -> p
      | Some prev_p -> prev_p));
  res
;;

let get_best_change_score monkeys =
  let stuff = Array.create ~len:Changes.num_combos 0 in
  let res = ref 0 in
  List.iter monkeys ~f:(fun monkey ->
    let changes = generate_changes monkey in
    Hashtbl.iteri changes ~f:(fun ~key ~data ->
      stuff.(key) <- stuff.(key) + data;
      res := Int.max !res stuff.(key)));
  !res
;;

let part1 s =
  String.split_lines s
  |> List.map ~f:Int.of_string
  |> List.sum (module Int) ~f:(fun n -> step_times n 2000)
  |> Int.to_string
;;

let part2 s =
  let monkeys = String.split_lines s |> List.map ~f:Int.of_string in
  get_best_change_score monkeys |> Int.to_string
;;

let%expect_test "changes" =
  print_s [%sexp (generate_changes ~steps:10 123 : int Hashtbl.M(Changes).t)];
  [%expect
    {|
    (((-3 6 -1 -1) 4) ((-1 -1 0 2) 6) ((-1 0 2 -2) 4) ((0 2 -2 0) 4)
     ((2 -2 0 -2) 2) ((6 -1 -1 0) 4))
    |}];
  ()
;;
(* List.iter (generate_changes ~steps:10 1) ~f:(fun (changes, p) ->
       Changes.show changes;
       print_s [%sexp (
   ) *)
