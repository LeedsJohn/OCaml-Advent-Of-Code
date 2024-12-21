open! Core
open! Helpers

(* type move =
    | Dir of Coordinate.t
    | Press *)

let num_pad =
  Board.of_string {|789
456
123
#0A|}
  |> Map.filter ~f:(fun c -> not (Char.equal c '#'))

let dir_pad =
  Board.of_string {|#^A
<v>|} |> Map.filter ~f:(fun c -> not (Char.equal c '#'))

let goal_dirs board start end_ =
  let start_pos =
    List.find_exn (Map.to_alist board) ~f:(fun (_, n) -> Char.equal n start)
    |> fst
  in
  let end_pos =
    List.find_exn (Map.to_alist board) ~f:(fun (_, n) -> Char.equal n end_)
    |> fst
  in
  let dx, dy = Coordinate.sub end_pos start_pos in
  if Coordinate.equal (dx, dy) (0, 0) then [ (0, 0) ]
  else
    let res =
      if dx > 0 then [ (1, 0) ] else if dx < 0 then [ (-1, 0) ] else []
    in
    if dy > 0 then (0, 1) :: res else if dy < 0 then (-1, 0) :: res else res

let num_goal_dirs start end_ = goal_dirs num_pad start end_
let dir_goal_dirs start end_ = goal_dirs dir_pad start end_

let dir_to_offset = function
  | '^' -> (0, -1)
  | 'v' -> (0, 1)
  | '<' -> (-1, 0)
  | '>' -> (1, 0)
  | _ -> raise_s [%message "bad dir char"]

let offset_to_dir = function
  | 0, -1 -> '^'
  | 0, 1 -> 'v'
  | -1, 0 -> '<'
  | 1, 0 -> '>'
  | _ -> raise_s [%message "bad offset"]

type move = Dir of Coordinate.t | Press

let all_moves = Press :: List.map Coordinate.offsets ~f:(fun thing -> Dir thing)

module Pad = struct
  type t =
    | Dir_pad of { pos : Coordinate.t; child : t }
    | Num_pad of { pos : Coordinate.t; message : string }
  [@@deriving sexp_of, equal, compare, hash]

  let rec apply_move t (m : move) =
    match m with
    | Dir d -> (
        match t with
        | Dir_pad { pos; child } ->
            let new_pos = Coordinate.add pos d in
            if Map.mem dir_pad new_pos then
              Some (Dir_pad { pos = new_pos; child })
            else None
        | Num_pad { pos; message } ->
            let new_pos = Coordinate.add pos d in
            if Map.mem num_pad new_pos then
              Some (Num_pad { pos = new_pos; message })
            else None)
    | Press -> (
        match t with
        | Num_pad { pos; message } ->
            Some
              (Num_pad
                 {
                   pos;
                   message =
                     message ^ (Map.find_exn num_pad pos |> String.of_char);
                 })
        | Dir_pad { pos; child } -> (
            let next_move =
              match Map.find_exn dir_pad pos with
              | 'A' -> Press
              | d -> Dir (dir_to_offset d)
            in
            match apply_move child next_move with
            | None -> None
            | Some new_child -> Some (Dir_pad { pos; child = new_child })))

  let neighbors t = List.filter_map all_moves ~f:(apply_move t)

  let rec get_message = function
    | Dir_pad { child; _ } -> get_message child
    | Num_pad { message; _ } -> message

  let is_solved t message =
    let cur_message = get_message t in
    String.equal message cur_message

  let good_message_so_far t message =
    let m = get_message t in
    String.is_prefix message ~prefix:m

  let start_pos num_bots =
    let num = Num_pad { pos = (2, 3); message = "" } in
    List.fold (List.range 0 num_bots) ~init:num ~f:(fun child _ ->
        Dir_pad { pos = (2, 0); child })
end

let shortest_path message num_bots =
  let visited = Hash_set.create (module Pad) in
  let longest = ref 0 in
  let rec aux ts steps =
    if List.exists ts ~f:(fun t -> Pad.is_solved t message) then steps
    else
      let neighbors =
        List.map ts ~f:Pad.neighbors
        |> List.join
        |> List.filter ~f:(fun t -> not (Hash_set.mem visited t))
        |> List.filter ~f:(fun t -> Pad.good_message_so_far t message)
      in
      List.iter neighbors ~f:(fun t ->
          longest := Int.max !longest (String.length (Pad.get_message t)));
      let neighbors =
        List.filter neighbors ~f:(fun t ->
            String.length (Pad.get_message t) + 1 >= !longest)
      in
      List.iter neighbors ~f:(fun t -> Hash_set.add visited t);
      aux neighbors (steps + 1)
  in
  aux [ Pad.start_pos num_bots ] 0

let shortest_path_greedy goal_message num_bots =
  let gd_mem = Hashtbl.create (module Pad) in
  let rec get_goal_dirs t =
    match Hashtbl.find gd_mem t with
    | Some stuff -> stuff
    | None ->
        let res =
          match t with
          | Pad.Num_pad { pos; message } ->
              let goal = String.get goal_message (String.length message) in
              num_goal_dirs (Map.find_exn num_pad pos) goal
          | Dir_pad { pos; child } ->
              List.map (get_goal_dirs child) ~f:(fun offset ->
                  match offset with
                  | 0, 0 -> dir_goal_dirs (Map.find_exn dir_pad pos) 'A'
                  | _ ->
                      dir_goal_dirs (Map.find_exn dir_pad pos)
                        (offset_to_dir offset))
              |> List.join
              |> Set.of_list (module Coordinate)
              |> Set.to_list
        in
        Hashtbl.add_exn gd_mem ~key:t ~data:res;
        res
  in
  let best_steps = ref Int.max_value in
  let memo_aux = Hashtbl.create (module Pad) in
  let rec aux t steps =
    match t with
    | None -> Int.max_value
    | Some t -> (
        if steps >= !best_steps then Int.max_value
        else
          match Hashtbl.find memo_aux t with
          | Some n -> n
          | None ->
              let res =
                if Pad.is_solved t goal_message then (
                  best_steps := steps;
                  steps)
                else
                  List.fold (get_goal_dirs t) ~init:Int.max_value
                    ~f:(fun acc dir ->
                      let move =
                        match dir with 0, 0 -> Press | _ -> Dir dir
                      in
                      Int.min acc (aux (Pad.apply_move t move) (steps + 1)))
              in
              Hashtbl.add_exn memo_aux ~key:t ~data:res;
              res)
  in
  aux (Some (Pad.start_pos num_bots)) 0

let complexity message num_bots =
  let l = shortest_path message num_bots in
  let num = String.filter message ~f:Char.is_digit |> Int.of_string in
  l * num

let complexity2 message num_bots =
  let l = shortest_path_greedy message num_bots in
  let num = String.filter message ~f:Char.is_digit |> Int.of_string in
  l * num

let part1 s =
  List.sum
    (module Int)
    (String.split_lines s)
    ~f:(fun line -> complexity line 2)
  |> Int.to_string |> Ok

let part2 s =
  List.sum
    (module Int)
    (String.split_lines s)
    ~f:(fun line -> complexity2 line 2)
  |> Int.to_string |> Ok

(* let%expect_test "" =
   print_s [%sexp (num_pad : char Board.t)];
   [%expect
     {|
       (((0 0) 7) ((0 1) 4) ((0 2) 1) ((1 0) 8) ((1 1) 5) ((1 2) 2) ((1 3) 0)
        ((2 0) 9) ((2 1) 6) ((2 2) 3) ((2 3) A))
       |}];
   print_s [%sexp (dir_pad : char Board.t)];
   [%expect {| (((0 1) <) ((1 0) ^) ((1 1) v) ((2 0) A) ((2 1) >)) |}];
   print_s [%sexp (Pad.start_pos : Pad.t)];
   [%expect
     {|
     (Dir_pad (pos (2 0))
      (child (Dir_pad (pos (2 0)) (child (Num_pad (pos (2 3)) (message ""))))))
     |}];
   print_s [%sexp (Pad.apply_move Pad.start_pos Press : Pad.t option)];
   [%expect
     {|
     ((Dir_pad (pos (2 0))
       (child (Dir_pad (pos (2 0)) (child (Num_pad (pos (2 3)) (message A)))))))
     |}];
   print_s [%sexp (Pad.apply_move Pad.start_pos (Dir (0, 1)) : Pad.t option)];
   [%expect
     {|
     ((Dir_pad (pos (2 1))
       (child (Dir_pad (pos (2 0)) (child (Num_pad (pos (2 3)) (message "")))))))
     |}];
   let mes =
     "<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A"
   in
   let t =
     String.fold mes ~init:(Some Pad.start_pos) ~f:(fun acc m ->
         match (acc, m) with
         | None, _ -> None
         | Some acc, 'A' -> Pad.apply_move acc Press
         | Some acc, c -> Pad.apply_move acc (Dir (dir_to_offset c)))
   in
   print_s [%sexp (t : Pad.t option)];
   [%expect
     {|
     ((Dir_pad (pos (2 0))
       (child (Dir_pad (pos (2 0)) (child (Num_pad (pos (2 3)) (message 029A)))))))
     |}];
   print_s [%sexp (shortest_path "0" : int)];
   [%expect {| 18 |}];
   () *)
