open! Core

module Stuff = struct
  type t = int Map.M(String).t Map.M(String).t [@@deriving hash, compare, sexp]

  let add_edge t p1 p2 dist =
    let t =
      Map.update t p1 ~f:(function
        | None -> Map.singleton (module String) p2 dist
        | Some m -> Map.add_exn m ~key:p2 ~data:dist)
    in
    Map.update t p2 ~f:(function
      | None -> Map.singleton (module String) p1 dist
      | Some m -> Map.add_exn m ~key:p1 ~data:dist)

  let remove (t : t) pos : t =
    let neighbors = Map.find_exn t pos in
    Map.keys neighbors
    |> List.fold ~init:(Map.remove t pos) ~f:(fun t next_pos ->
           Map.update t next_pos ~f:(function
             | None -> raise_s [%message "impossible"]
             | Some m -> Map.remove m pos))

  let of_string s : t =
    String.split_lines s
    |> List.fold
         ~init:(Map.empty (module String))
         ~f:(fun acc line ->
           let words = String.split line ~on:' ' |> List.to_array in
           let p1, p2, dist = (words.(0), words.(2), Int.of_string words.(4)) in
           add_edge acc p1 p2 dist)

  let shortest_distance_to_all t start_pos =
    let rec aux (t : t) (cur_dist : int) (pos : string) =
      match Map.length t with
      | 1 -> Some cur_dist
      | _ ->
          let neighbors = Map.find_exn t pos in
          let t = remove t pos in
          Map.fold neighbors ~init:None ~f:(fun ~key:next_pos ~data:dist acc ->
              match aux t (cur_dist + dist) next_pos with
              | None -> acc
              | Some n ->
                  if n < Option.value acc ~default:Int.max_value then Some n
                  else acc)
    in
    aux t 0 start_pos
end

let part1 s =
  let graph = Stuff.of_string s in
  let places = Map.keys graph in
  List.filter_map places ~f:(Stuff.shortest_distance_to_all graph)
  |> List.min_elt ~compare:Int.compare
  |> Option.value_exn |> Int.to_string |> Ok

let part2 s =
  let graph =
    Stuff.of_string s |> Map.map ~f:(fun m -> Map.map m ~f:(fun n -> -n))
  in
  let places = Map.keys graph in
  List.filter_map places ~f:(Stuff.shortest_distance_to_all graph)
  |> List.min_elt ~compare:Int.compare
  |> Option.value_exn |> Int.( * ) (-1) |> Int.to_string |> Ok

let%expect_test "stuff" =
  let s =
    {|London to Dublin = 464
London to Belfast = 518
Dublin to Belfast = 141|}
  in
  let g = Stuff.of_string s in
  print_s [%sexp (g : Stuff.t)];
  [%expect
    {|
    ((Belfast ((Dublin 141) (London 518))) (Dublin ((Belfast 141) (London 464)))
     (London ((Belfast 518) (Dublin 464))))
    |}];
  print_s [%sexp (Stuff.remove g "London" : Stuff.t)];
  [%expect {| ((Belfast ((Dublin 141))) (Dublin ((Belfast 141)))) |}];
  ()

(* 
memoized version was ~2 times faster

module John = struct
  type t = string * Stuff.t [@@deriving sexp_of, hash, compare]
end

let memo = Hashtbl.create (module John)

let shortest_distance_to_all t start_pos =
  let rec aux t pos =
    match Map.length t with
    | 1 -> Some 0
    | _ -> (
        let key = (pos, t) in
        match Hashtbl.find memo key with
        | Some res -> res
        | None ->
            let res =
              let neighbors = Map.find_exn t pos in
              let t = Stuff.remove t pos in
              Map.fold neighbors ~init:None
                ~f:(fun ~key:next_pos ~data:dist acc ->
                  match aux t next_pos with
                  | None -> acc
                  | Some n ->
                      if n + dist < Option.value acc ~default:Int.max_value then
                        Some (n + dist)
                      else acc)
            in
            Hashtbl.add_exn memo ~key ~data:res;
            res)
  in
  aux t start_pos
 *)
