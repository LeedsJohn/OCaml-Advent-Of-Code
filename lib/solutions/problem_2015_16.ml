open! Core

module Aunt_sue = struct
  type t =
    { id : int
    ; stats : int Map.M(String).t
    }

  let of_string s =
    let ar = String.split s ~on:' ' |> List.to_array in
    let id = String.drop_suffix ar.(1) 1 |> Int.of_string in
    let stats =
      List.fold
        [ 2; 4; 6 ]
        ~init:(Map.empty (module String))
        ~f:(fun acc i ->
          let key = String.drop_suffix ar.(i) 1 in
          let data =
            String.chop_suffix_if_exists ar.(i + 1) ~suffix:"," |> Int.of_string
          in
          Map.add_exn acc ~key ~data)
    in
    { id; stats }
  ;;

  let can_be_her { stats; _ } =
    List.for_all
      [ "children", 3
      ; "cats", 7
      ; "samoyeds", 2
      ; "pomeranians", 3
      ; "akitas", 0
      ; "vizslas", 0
      ; "goldfish", 5
      ; "trees", 3
      ; "cars", 2
      ; "perfumes", 1
      ]
      ~f:(fun (key, data) ->
        match Map.find stats key with
        | None -> true
        | Some d -> data = d)
  ;;

  let can_be_her2 { stats; _ } =
    List.for_all
      [ ("children", 3, fun n1 n2 -> n1 = n2)
      ; ("cats", 7, fun n1 n2 -> n1 > n2)
      ; ("samoyeds", 2, fun n1 n2 -> n1 = n2)
      ; ("pomeranians", 3, fun n1 n2 -> n1 < n2)
      ; ("akitas", 0, fun n1 n2 -> n1 = n2)
      ; ("vizslas", 0, fun n1 n2 -> n1 = n2)
      ; ("goldfish", 5, fun n1 n2 -> n1 < n2)
      ; ("trees", 3, fun n1 n2 -> n1 > n2)
      ; ("cars", 2, fun n1 n2 -> n1 = n2)
      ; ("perfumes", 1, fun n1 n2 -> n1 = n2)
      ]
      ~f:(fun (key, data, fn) ->
        match Map.find stats key with
        | None -> true
        | Some d -> fn d data)
  ;;
end

let part1 s =
  let ({ id; _ } : Aunt_sue.t) =
    String.split_lines s
    |> List.map ~f:Aunt_sue.of_string
    |> List.find_exn ~f:Aunt_sue.can_be_her
  in
  Int.to_string id
;;

let part2 s =
  let ({ id; _ } : Aunt_sue.t) =
    String.split_lines s
    |> List.map ~f:Aunt_sue.of_string
    |> List.find_exn ~f:Aunt_sue.can_be_her2
  in
  Int.to_string id
;;
