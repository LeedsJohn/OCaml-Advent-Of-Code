open Core

let day = 25
let year = 2023

module Graph = struct
  module String_set = Set.Make (String)
  module String_map = Map.Make (String)

  type t = String_set.t String_map.t [@@deriving sexp]

  let empty = String_map.empty
  let num_vertices t = Map.length t

  let add_edge t v1 v2 =
    let aux t start stop =
      Map.update t start ~f:(fun edges ->
          let cur_edges = Option.value edges ~default:String_set.empty in
          Set.add cur_edges stop)
    in
    if String.is_empty v1 || String.is_empty v2 then t
    else
      let t = aux t v1 v2 in
      aux t v2 v1

  let remove_edge t v1 v2 =
    let aux t start stop =
      Map.change t start ~f:(fun edges ->
          match edges with
          | None -> None
          | Some edges -> Some (Set.remove edges stop))
    in
    let t = aux t v1 v2 in
    aux t v2 v1

  let count_graph_section_size t start_pos =
    let rec aux (visited : String_set.t) cur_state =
      Set.fold (Map.find_exn t cur_state) ~init:(1, visited)
        ~f:(fun (count, visited) next_state ->
          if Set.mem visited next_state then (count, visited)
          else
            let new_count, visited =
              aux (Set.add visited next_state) next_state
            in
            (count + new_count, visited))
    in
    let res = aux (String_set.singleton start_pos) start_pos |> fst in
    res

  let is_fully_connected (t : t) =
    count_graph_section_size t (Map.nth_exn t 0 |> fst) = Map.length t

  let shortest_path_between (t : t) v1 v2 =
    let visited = Hash_set.create (module String) in
    let q = Queue.singleton [ v1 ] in
    Hash_set.add visited v1;
    let res = ref None in
    while Option.is_none !res do
      let cur_path = Queue.dequeue_exn q in
      let neighbors = Map.find_exn t (List.hd_exn cur_path) in
      Set.iter neighbors ~f:(fun next_node ->
          let new_path = next_node :: cur_path in
          if String.(next_node = v2) then res := Some new_path;
          if not (Hash_set.mem visited next_node) then (
            Hash_set.add visited next_node;
            Queue.enqueue q new_path))
    done;
    Option.value_exn !res

  let n_most_popular t n =
    let counts = Hashtbl.create (module String) in
    let get_random_vertex () = List.random_element_exn (Map.keys t) in
    for _ = 0 to 10000 do
      let v1, v2 = (get_random_vertex (), get_random_vertex ()) in
      List.iter (shortest_path_between t v1 v2) ~f:(fun s ->
          Hashtbl.update counts s ~f:(fun n -> Option.value n ~default:0 + 1))
    done;
    let most_popular =
      Hashtbl.to_alist counts
      |> List.sort ~compare:(fun (_, n1) (_, n2) -> Int.compare n2 n1)
    in
    List.slice most_popular 0 n |> List.map ~f:(fun (name, _) -> name)

  let edges_to_remove t =
    let vertices = n_most_popular t 10 in
    let edge_list name =
      Map.find_exn t name
      |> Set.fold ~init:[] ~f:(fun acc n -> (name, n) :: acc)
    in
    let edges = List.concat_map vertices ~f:edge_list in
    let rec aux t removed_edges =
      if List.length removed_edges = 3 then
        if not (is_fully_connected t) then Some removed_edges else None
      else
        List.find_map edges ~f:(fun (v1, v2) ->
            if
              List.mem removed_edges (v1, v2) ~equal:(fun (a1, b1) (a2, b2) ->
                  String.(a1 = a2 && b1 = b2))
            then None
            else aux (remove_edge t v1 v2) ((v1, v2) :: removed_edges))
    in
    aux t []
end

let parse_text text =
  String.strip text |> String.split_lines
  |> List.fold ~init:Graph.empty ~f:(fun acc line ->
         let line = String.split line ~on:':' in
         let start_pos = List.hd_exn line in
         let next_nodes =
           List.nth_exn line 1 |> String.split ~on:' '
           |> List.filter ~f:(fun s -> not (String.is_empty s))
         in
         List.fold next_nodes ~init:acc ~f:(fun acc next_node ->
             Graph.add_edge acc start_pos next_node))

(* Because we know this graph is almost in two separate parts, we can select
   two vertices and find the second path between them and the "bridge" nodes
   will be more likely to appear in the path.  If we repeat this process enough
   times and find the nodes that appear most frequently, the edges we need to
   remove will include these vertices.
*)
let part1 fname =
  let t = parse_text (In_channel.read_all fname) in
  let edges_to_remove = Graph.edges_to_remove t |> Option.value_exn in
  let t =
    List.fold edges_to_remove ~init:t ~f:(fun acc (v1, v2) ->
        Graph.remove_edge acc v1 v2)
  in
  let section_length =
    Graph.count_graph_section_size t (Map.nth_exn t 0 |> fst)
  in
  section_length * (Graph.num_vertices t - section_length) |> Int.to_string

let part2 _ = "there was no part 2"
