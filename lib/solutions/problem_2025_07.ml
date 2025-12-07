open! Core

let part1 s =
  let lines = String.split_lines s in
  let start_i =
    String.findi (List.hd_exn lines) ~f:(fun _ c -> Char.equal c 'S')
    |> Option.value_exn
    |> fst
    |> Set.singleton (module Int)
  in
  let rec solve acc positions = function
    | [] -> acc
    | hd :: tl ->
      let splitters_hit, next_positions =
        Set.fold
          positions
          ~init:(0, Set.empty (module Int))
          ~f:(fun (sh, np) pos ->
            if Char.equal (String.get hd pos) '.'
            then sh, Set.add np pos
            else sh + 1, List.fold [ pos - 1; pos + 1 ] ~init:np ~f:Set.add)
      in
      solve (acc + splitters_hit) next_positions tl
  in
  solve 0 start_i (List.tl_exn lines) |> Int.to_string
;;

let part2 s =
  let lines = String.split_lines s |> Array.of_list |> Array.map ~f:String.to_array in
  let start_i =
    Array.findi lines.(0) ~f:(fun _ c -> Char.equal c 'S') |> Option.value_exn |> fst
  in
  let memo =
    Array.init (Array.length lines) ~f:(fun _ ->
      Array.create ~len:(Array.length lines.(0)) None)
  in
  let rec solve r c =
    if r + 1 >= Array.length lines
    then 1
    else (
      match memo.(r).(c) with
      | Some n -> n
      | None ->
        let res =
          if Char.( = ) lines.(r + 1).(c) '.'
          then solve (r + 1) c
          else solve (r + 1) (c - 1) + solve (r + 1) (c + 1)
        in
        memo.(r).(c) <- Some res;
        res)
  in
  solve 0 start_i |> Int.to_string
;;
