open! Core

let mod_num = 16777216

let step n =
  let do_f f n = Int.( % ) (Int.bit_xor (f n) n) mod_num in
  do_f (fun n -> n * 64) n |> do_f (fun n -> n / 32) |> do_f (fun n -> n * 2048)

let step_times n times =
  List.fold (List.range 0 times) ~init:n ~f:(fun acc _ -> step acc)

module Changes = struct
  module T = struct
    type t = int * int * int * int [@@deriving compare, equal, hash, sexp]
  end

  include Comparator.Make (T)
  include T

  let add_new_dif (_, n2, n3, n4) d = (n2, n3, n4, d)
end

let generate_changes ?(steps = 2000) n =
  let prices =
    n
    :: (List.fold_map
          (List.range 0 (steps - 1))
          ~init:n
          ~f:(fun acc _ ->
            let res = step acc in
            (res, res))
       |> snd)
  in
  let prices = List.map prices ~f:(fun n -> Int.( % ) n 10) in
  let changes =
    match prices with
    | a :: b :: c :: d :: e :: tl ->
        let changes = (b - a, c - b, d - c, e - d) in
        (changes, e)
        :: (List.fold_map tl ~init:(changes, e)
              ~f:(fun (changes, prev_num) price ->
                let changes = Changes.add_new_dif changes (price - prev_num) in
                ((changes, price), (changes, price)))
           |> snd)
    | prices -> raise_s [%message "not enough prices" (prices : int list)]
  in
  List.fold changes
    ~init:(Map.empty (module Changes))
    ~f:(fun acc (changes, p) ->
      Map.update acc changes ~f:(function None -> p | Some prev_p -> prev_p))

let get_best_change_score monkeys =
  let values =
    List.fold monkeys
      ~init:(Map.empty (module Changes))
      ~f:(fun acc monkey ->
        let changes = generate_changes monkey in
        Map.merge acc changes ~f:(fun ~key:_ values ->
            match values with
            | `Both (n1, n2) -> Some (n1 + n2)
            | `Left n | `Right n -> Some n))
  in
  Map.data values |> List.max_elt ~compare:Int.compare |> Option.value_exn

let part1 s =
  String.split_lines s |> List.map ~f:Int.of_string
  |> List.sum (module Int) ~f:(fun n -> step_times n 2000)
  |> Int.to_string |> Ok

let part2 s =
  let monkeys = String.split_lines s |> List.map ~f:Int.of_string in
  get_best_change_score monkeys |> Int.to_string |> Ok

let%expect_test "step" =
  print_s [%sexp (step 123 : int)];
  [%expect {|15887950|}];
  ()
