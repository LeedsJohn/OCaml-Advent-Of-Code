open Core

let day = 5
let year = 2023

let parse_input fname =
  let lines = In_channel.read_lines fname |> Array.of_list in
  let seeds =
    lines.(0) |> String.split ~on:' ' |> List.tl_exn
    |> List.map ~f:Int.of_string
  in
  let maps =
    Array.fold
      (Array.slice lines 2 (Array.length lines))
      ~init:[]
      ~f:(fun acc line ->
        if String.is_empty line then acc
        else if not (Char.is_digit (String.get line 0)) then [] :: acc
        else
          let nums =
            String.split line ~on:' ' |> List.map ~f:Int.of_string
            |> Array.of_list
          in
          ((nums.(0), nums.(1), nums.(2)) :: List.hd_exn acc) :: List.tl_exn acc)
  in
  (seeds, List.rev maps)

let find_dest seed ranges =
  let res =
    List.find_map ranges ~f:(fun (dest, source, range) ->
        if seed >= source + range || seed < source then None
        else Some (dest + (seed - source)))
  in
  match res with None -> seed | Some n -> n

let get_seed_val seed maps =
  List.fold maps ~init:seed ~f:(fun acc map -> find_dest acc map)

let part1 fname =
  let seeds, maps = parse_input fname in
  List.map seeds ~f:(fun seed -> get_seed_val seed maps)
  |> List.min_elt ~compare:(fun n1 n2 -> n1 - n2)
  |> Option.value_exn |> Int.to_string

(* this took like 30 mins to run LOL *)
let part2 fname =
  let seeds, maps = parse_input fname in
  let seeds =
    List.chunks_of seeds ~length:2
    |> List.map ~f:(fun group ->
           (List.hd_exn group, List.hd_exn (List.tl_exn group)))
  in
  let get_min_val seed range =
    List.fold
      (List.range seed (seed + range))
      ~init:Int.max_value
      ~f:(fun acc s -> Int.min acc (get_seed_val s maps))
  in
  List.mapi seeds ~f:(fun i (seed, range) ->
      print_endline (Int.to_string i ^ "\n");
      get_min_val seed range)
  |> List.min_elt ~compare:(fun n1 n2 -> n1 - n2)
  |> Option.value_exn |> Int.to_string
