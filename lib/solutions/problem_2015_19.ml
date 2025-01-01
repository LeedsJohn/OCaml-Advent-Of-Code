open! Core

let of_string s =
  let lines = String.split_lines s in
  let replacements =
    List.fold lines
      ~init:(Map.empty (module String))
      ~f:(fun acc line ->
        if not (String.mem line '>') then acc
        else
          let stuff = String.split line ~on:' ' |> List.to_array in
          Map.update acc stuff.(0) ~f:(function
            | None -> [ stuff.(2) ]
            | Some l -> stuff.(2) :: l))
  in
  let s =
    List.find_exn lines ~f:(fun line ->
        String.length line > 1 && not (String.mem line '>'))
  in
  (replacements, s)

let invert_replacements replacements =
  Map.fold replacements
    ~init:(Map.empty (module String))
    ~f:(fun ~key:original ~data:replacements acc ->
      List.fold replacements ~init:acc ~f:(fun acc replacement ->
          Map.update acc replacement ~f:(function
            | None -> [ original ]
            | Some l -> original :: l)))

let get_molecules s replacements =
  let get_one_replacement original replaced =
    String.substr_index_all s ~may_overlap:true ~pattern:original
    |> List.map ~f:(fun i ->
           let before = if i = 0 then "" else String.slice s 0 i in
           let after = String.slice s (i + String.length original) 0 in
           List.map replaced ~f:(fun replaced -> before ^ replaced ^ after))
    |> List.join
    |> Set.of_list (module String)
  in
  Map.fold replacements
    ~init:(Set.empty (module String))
    ~f:(fun ~key:original ~data:replaced acc ->
      Set.union acc (get_one_replacement original replaced))

let bfs replacements start_string goal_string =
  let rec aux cur steps =
    if Set.mem cur goal_string then steps
    else if Set.length cur = 0 then Int.max_value
    else
      let neighbors =
        Set.to_list cur
        |> List.map ~f:(fun s -> get_molecules s replacements)
        |> Set.union_list (module String)
      in
      aux neighbors (steps + 1)
  in
  aux (Set.singleton (module String) start_string) 0

let greedy_bfs replacements start_string goal_string =
  let rec aux cur steps =
    if Set.mem cur goal_string then steps
    else if Set.length cur = 0 then Int.max_value
    else
      let neighbors =
        Set.to_list cur
        |> List.map ~f:(fun s -> get_molecules s replacements)
        |> Set.union_list (module String)
      in
      let min_length =
        Set.fold neighbors ~init:Int.max_value ~f:(fun acc s ->
            Int.min acc (String.length s))
      in
      let neighbors =
        Set.filter neighbors ~f:(fun s -> String.length s = min_length)
      in
      aux neighbors (steps + 1)
  in
  aux (Set.singleton (module String) start_string) 0

let part1 s =
  let replacements, s = of_string s in
  get_molecules s replacements |> Set.length |> Int.to_string |> Ok

let part2 s =
  let replacements, s = of_string s in
  let replacements = invert_replacements replacements in
  greedy_bfs replacements s "e" |> Int.to_string |> Ok
