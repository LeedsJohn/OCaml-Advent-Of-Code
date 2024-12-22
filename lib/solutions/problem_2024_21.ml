open! Core
open! Helpers

let offset_to_dir = function
  | 0, -1 -> '^'
  | 0, 1 -> 'v'
  | -1, 0 -> '<'
  | 1, 0 -> '>'
  | _ -> raise_s [%message "bad offset"]

let num_pad =
  Board.of_string {|789
456
123
#0A|}
  |> Map.filter ~f:(fun c -> not (Char.equal c '#'))

let dir_pad =
  Board.of_string {|#^A
<v>|} |> Map.filter ~f:(fun c -> not (Char.equal c '#'))

let path_to_dirs path =
  let rec aux acc path =
    match path with
    | a :: b :: tl ->
        let new_dir = offset_to_dir (Coordinate.sub b a) in
        aux (new_dir :: acc) (b :: tl)
    | _ -> List.rev acc
  in
  aux [] path

let all_shortest_paths board start end_ =
  let start_pos =
    List.find_exn (Map.to_alist board) ~f:(fun (_, n) -> Char.equal n start)
    |> fst
  in
  let end_pos =
    List.find_exn (Map.to_alist board) ~f:(fun (_, n) -> Char.equal n end_)
    |> fst
  in
  let rec aux (level : Coordinate.t list list) =
    let ends_in_goal =
      List.filter level ~f:(fun path ->
          Coordinate.equal end_pos (List.hd_exn path))
    in
    if List.length ends_in_goal <> 0 then List.map ends_in_goal ~f:List.rev
    else
      let next_level =
        List.map level ~f:(fun path ->
            List.filter_map
              (Coordinate.neighbors (List.hd_exn path))
              ~f:(fun pos ->
                if Map.mem board pos then Some (pos :: path) else None))
        |> List.join
      in
      aux next_level
  in
  let paths = aux [ [ start_pos ] ] in
  List.map paths ~f:path_to_dirs

let num_pad_shortest_paths start end_ = all_shortest_paths num_pad start end_
let dir_pad_shortest_paths start end_ = all_shortest_paths dir_pad start end_

module Inp = struct
  type t = char * char * int [@@deriving sexp, compare, equal, hash]
end

let get_start_stops path = List.zip_exn ('A' :: path) (path @ [ 'A' ])
let sp_memo = Hashtbl.create (module Inp)

let rec shortest_path (start_pos : char) (end_pos : char) (r : int)
    (num_robots : int) : int =
  if r = num_robots then 1
  else
    let k = (start_pos, end_pos, r) in
    match Hashtbl.find sp_memo k with
    | Some res -> res
    | None ->
        let all_paths =
          if r = 0 then num_pad_shortest_paths start_pos end_pos
          else dir_pad_shortest_paths start_pos end_pos
        in
        let res =
          List.fold all_paths ~init:Int.max_value ~f:(fun acc path ->
              let num_steps =
                List.sum
                  (module Int)
                  (get_start_stops path)
                  ~f:(fun (start_pos, end_pos) ->
                    shortest_path start_pos end_pos (r + 1) num_robots)
              in
              Int.min acc num_steps)
        in
        Hashtbl.add_exn sp_memo ~key:k ~data:res;
        res

let message_cost message num_bots =
  String.foldi message
    ~init:(shortest_path 'A' (String.get message 0) 0 num_bots)
    ~f:(fun i acc c ->
      if i + 1 = String.length message then acc
      else acc + shortest_path c (String.get message (i + 1)) 0 num_bots)

let complexity message num_bots =
  let l = message_cost message num_bots in
  let num = String.filter message ~f:Char.is_digit |> Int.of_string in
  l * num

let part1 s =
  List.sum
    (module Int)
    (String.split_lines s)
    ~f:(fun line -> complexity line 3)
  |> Int.to_string |> Ok

let part2 s =
  List.sum
    (module Int)
    (String.split_lines s)
    ~f:(fun line -> complexity line 26)
  |> Int.to_string |> Ok

let%expect_test "" =
  print_s [%sexp (num_pad_shortest_paths 'A' '2' : char list list)];
  [%expect {| ((< ^) (^ <)) |}];
  print_s [%sexp (shortest_path 'A' '0' 0 3 : int)];
  [%expect {| 18 |}];
  ()
