open! Core
open! Helpers

let of_string s = Board.of_string s |> Map.map ~f:(Char.equal '#')

let count_neighbors t pos =
  List.count (Coordinate.neighbors8 pos) ~f:(fun pos ->
    Map.find t pos |> Option.value ~default:false)
;;

let step t =
  Map.mapi t ~f:(fun ~key:pos ~data:is_on ->
    let num_neighbors_on = count_neighbors t pos in
    if is_on then num_neighbors_on = 2 || num_neighbors_on = 3 else num_neighbors_on = 3)
;;

let step2 t =
  let t = step t in
  List.fold
    [ 0, 0; 0, 99; 99, 0; 99, 99 ]
    ~init:t
    ~f:(fun acc key -> Map.set acc ~key ~data:true)
;;

let make_step_n_times step_fn =
  fun t n -> List.fold (List.range 0 n) ~init:t ~f:(fun acc _ -> step_fn acc)
;;

let step_n_times = make_step_n_times step
let step2_n_times = make_step_n_times step2
let num_lights_on t = Map.data t |> List.count ~f:Fn.id

let part1 s =
  let t = of_string s in
  step_n_times t 100 |> num_lights_on |> Int.to_string
;;

let part2 s =
  let t = of_string s in
  step2_n_times t 100 |> num_lights_on |> Int.to_string
;;
