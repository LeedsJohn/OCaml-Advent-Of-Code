open! Core
open! Helpers

let of_string s =
  let lines = String.split_lines s in
  let replacements =
    List.fold
      lines
      ~init:(Map.empty (module String))
      ~f:(fun acc line ->
        if not (String.mem line '>')
        then acc
        else (
          let stuff = String.split line ~on:' ' |> List.to_array in
          Map.update acc stuff.(0) ~f:(function
            | None -> [ stuff.(2) ]
            | Some l -> stuff.(2) :: l)))
  in
  let s =
    List.find_exn lines ~f:(fun line ->
      String.length line > 1 && not (String.mem line '>'))
  in
  replacements, s
;;

let invert_replacements replacements =
  Map.fold
    replacements
    ~init:(Map.empty (module String))
    ~f:(fun ~key:original ~data:replacements acc ->
      List.fold replacements ~init:acc ~f:(fun acc replacement ->
        Map.update acc replacement ~f:(function
          | None -> [ original ]
          | Some l -> original :: l)))
;;

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
  Map.fold
    replacements
    ~init:(Set.empty (module String))
    ~f:(fun ~key:original ~data:replaced acc ->
      Set.union acc (get_one_replacement original replaced))
;;

module Stuff = struct
  type t = int * string * int [@@deriving sexp_of]

  let compare (n1, _, _) (n2, _, _) = Int.compare n1 n2
end

module Pq = Priority_queue.Make (Stuff)

let greedy replacements start_string goal_string =
  let visited = Hash_set.create (module String) in
  Hash_set.add visited start_string;
  let rec aux pq =
    let (_, cur, steps), pq = Pq.get_exn pq in
    if String.equal cur goal_string
    then Some steps
    (* else if String.length cur >= String.length goal_string then None *)
    else (
      let neighbors =
        get_molecules cur replacements
        |> Set.to_list
        |> List.filter ~f:(fun s -> not (Hash_set.mem visited s))
      in
      List.iter neighbors ~f:(Hash_set.add visited);
      let pq =
        List.fold neighbors ~init:pq ~f:(fun acc s ->
          Pq.add acc (String.length s, s, steps + 1))
      in
      aux pq)
  in
  aux (Pq.singleton (String.length start_string, start_string, 0)) |> Option.value_exn
;;

let part1 s =
  let replacements, s = of_string s in
  get_molecules s replacements |> Set.length |> Int.to_string
;;

let part2 s =
  let replacements, s = of_string s in
  let replacements = invert_replacements replacements in
  greedy replacements s "e" |> Int.to_string
;;
