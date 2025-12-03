open! Core

let get_thing s =
  let lines = String.split_lines s in
  let count_rows lines i =
    List.count lines ~f:(fun line -> Char.equal (String.get line i) '#')
  in
  let res =
    if String.equal (List.hd_exn lines) "#####"
    then `Lock (List.map (List.range 0 5) ~f:(count_rows lines))
    else `Key (List.map (List.range 0 5) ~f:(count_rows lines))
  in
  (* print_s
    [%message
      "" (lines : string list) (res : [ `Key of int list | `Lock of int list ])]; *)
  res
;;

let of_string s =
  let stuff =
    String.substr_replace_all s ~pattern:"\n\n" ~with_:"*" |> String.split ~on:'*'
  in
  List.fold stuff ~init:([], []) ~f:(fun (keys, locks) thing ->
    match get_thing thing with
    | `Key s -> s :: keys, locks
    | `Lock s -> keys, s :: locks)
;;

let count_stuff locks keys pred =
  List.cartesian_product locks keys
  |> List.count ~f:(fun (lock, key) ->
    List.zip_exn lock key |> List.for_all ~f:(fun (l, k) -> pred l k))
;;

let part1 s =
  let keys, locks = of_string s in
  count_stuff locks keys (fun l k -> l + k <= 7) |> Int.to_string
;;

let part2 _ = raise_s [%message "unimplemented"]
