open Core

let day = 3
let year = 2023

let parse_input lines =
  String.split_lines lines |> List.map ~f:String.to_array |> Array.of_list

let rec check_num board x y =
  if x = Array.length board.(0) || not (Char.is_digit board.(y).(x)) then false
  else
    let neighbors =
      List.cartesian_product [ -1; 0; 1 ] [ -1; 0; 1 ]
      |> List.map ~f:(fun (dx, dy) -> (x + dx, y + dy))
      |> List.filter ~f:(fun (x, y) ->
             0 <= x
             && x < Array.length board.(0)
             && 0 <= y
             && y < Array.length board)
    in
    if
      List.exists neighbors ~f:(fun (x, y) ->
          (not (Char.is_digit board.(y).(x)))
          && not (Char.equal '.' board.(y).(x)))
    then true
    else check_num board (x + 1) y

let get_num board x y =
  if not (check_num board x y) then 0
  else
    let res = ref "" in
    let stop = ref false in
    let x = ref x in
    while not !stop do
      if !x >= Array.length board.(0) || not (Char.is_digit board.(y).(!x)) then
        stop := true
      else (
        res := !res ^ String.of_char board.(y).(!x);
        board.(y).(!x) <- '.');
      x := !x + 1
    done;
    Int.of_string !res

let part1 fname =
  let board = parse_input (In_channel.read_all fname) in
  let res = ref 0 in
  List.iter
    (List.range 0 (Array.length board))
    ~f:(fun y ->
      List.iter
        (List.range 0 (Array.length board.(0)))
        ~f:(fun x ->
          if check_num board x y then res := !res + get_num board x y));
  Int.to_string !res

module Num_pair = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp_of]
  end

  include T
  include Comparator.Make (T)
end

let get_number_start_pos board x y =
  List.cartesian_product [ -1; 0; 1 ] [ -1; 0; 1 ]
  |> List.map ~f:(fun (dx, dy) -> (x + dx, y + dy))
  |> List.filter ~f:(fun (x, y) ->
         0 <= x
         && x < Array.length board.(0)
         && 0 <= y
         && y < Array.length board
         && Char.is_digit board.(y).(x))
  |> List.map ~f:(fun (x, y) ->
         let x = ref x in
         while !x >= 0 && Char.is_digit board.(y).(!x) do
           x := !x - 1
         done;
         (!x + 1, y))
  |> Set.of_list (module Num_pair)
  |> Set.to_list

let get_gear_ratio board x y =
  let start_positions = get_number_start_pos board x y in
  if List.length start_positions <> 2 then 0
  else
    let x1, y1 = List.hd_exn start_positions in
    let x2, y2 = List.hd_exn (List.tl_exn start_positions) in
    get_num board x1 y1 * get_num board x2 y2

(* Note: this is potentially incorrect because it assumes that each number
   can only belong to one * sign *)
let part2 fname =
  let board = parse_input (In_channel.read_all fname) in
  let res = ref 0 in
  List.iter
    (List.range 0 (Array.length board))
    ~f:(fun y ->
      List.iter
        (List.range 0 (Array.length board.(0)))
        ~f:(fun x ->
          if Char.equal board.(y).(x) '*' then
            res := !res + get_gear_ratio board x y));
  Int.to_string !res
