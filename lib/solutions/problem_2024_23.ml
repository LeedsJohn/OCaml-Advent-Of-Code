open! Core

module Lan_party = struct
  type t = Set.M(String).t Map.M(String).t

  let of_string s : t =
    let add t a b =
      Map.update t a ~f:(function
        | None -> Set.singleton (module String) b
        | Some s -> Set.add s b)
    in
    String.split_lines s
    |> List.fold
         ~init:(Map.empty (module String))
         ~f:(fun acc line ->
           let a = String.slice line 0 2 in
           let b = String.slice line 3 5 in
           let acc = add acc a b in
           add acc b a)

  let is_all_connected t points =
    List.length points <= 1
    || List.cartesian_product points points
       |> List.for_all ~f:(fun (pt1, pt2) ->
              String.equal pt1 pt2 || Set.mem (Map.find_exn t pt1) pt2)

  let get_connected_groups_of_n t n =
    let rec dfs cur points =
      if List.length cur = n then if is_all_connected t cur then [ cur ] else []
      else
        match points with
        | [] -> []
        | hd :: tl -> dfs (hd :: cur) tl @ dfs cur tl
    in
    dfs [] (Map.keys t)

  let get_largest_connected_group t =
    let rec aux cur = function
      | [] -> cur
      | hd :: tl ->
          let group1 =
            let neighbors = Map.find_exn t hd in
            if List.for_all cur ~f:(fun point -> Set.mem neighbors point) then
              aux (hd :: cur) tl
            else []
          in
          let group2 = aux cur tl in
          if List.length group1 > List.length group2 then group1 else group2
    in
    aux [] (Map.keys t)
end

let part1 s =
  let t = Lan_party.of_string s in
  let groups = Lan_party.get_connected_groups_of_n t 3 in
  List.count groups ~f:(fun group ->
      List.exists group ~f:(fun pt -> String.is_prefix pt ~prefix:"t"))
  |> Int.to_string |> Ok

let part2 s =
  let t = Lan_party.of_string s in
  let group = Lan_party.get_largest_connected_group t in
  List.sort group ~compare:String.compare |> String.concat ~sep:"," |> Ok
