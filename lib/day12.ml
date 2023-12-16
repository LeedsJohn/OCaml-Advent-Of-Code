open! Core

let day = 12
let year = 2023

let parse_text text =
  String.split_lines text
  |> List.map ~f:(fun line ->
         let line = String.split line ~on:' ' in
         let row = List.hd_exn line in
         let nums =
           List.map
             (List.tl_exn line |> List.hd_exn |> String.split ~on:',')
             ~f:Int.of_string
           |> Array.of_list
         in
         (row, nums))

module State = struct
  type t = int * int [@@deriving compare, hash, sexp_of]
end

let solve_line row nums =
  let get_next_not_period i =
    List.find
      (List.range i (String.length row))
      ~f:(fun i -> Char.(String.unsafe_get row i <> '.'))
    |> Option.value ~default:(String.length row)
  in
  let can_fill_segment i j =
    let num = nums.(j) in
    i + num <= String.length row
    && List.for_all
         (List.range i (i + num))
         ~f:(fun i -> Char.(String.unsafe_get row i <> '.'))
    && (i + num = String.length row
       || Char.(String.unsafe_get row (i + num) <> '#'))
  in
  let memo = Hashtbl.create (module State) in
  let rec aux i j =
    match Hashtbl.find memo (i, j) with
    | Some n -> n
    | None ->
        let res =
          match (i = String.length row, j = Array.length nums) with
          | true, true -> 1
          | true, false -> 0
          | false, true ->
              List.for_all
                (List.range i (String.length row))
                ~f:(fun i -> Char.(String.unsafe_get row i <> '.'))
              |> Bool.to_int
          | false, false ->
              let sub_period =
                if Char.(String.unsafe_get row i <> '?') then 0
                else aux (get_next_not_period (i + 1)) j
              in
              let fill_segment =
                if not (can_fill_segment i j) then 0
                else aux (get_next_not_period (i + nums.(j) + 1)) (j + 1)
              in
              sub_period + fill_segment
        in
        Hashtbl.add_exn memo ~key:(i, j) ~data:res;
        res
  in

  aux (get_next_not_period 0) 0

let part1 fname =
  let input = parse_text (In_channel.read_all fname) in
  List.fold input ~init:0 ~f:(fun acc (row, nums) -> acc + solve_line row nums)
  |> Int.to_string

let part2 fname =
  let input = parse_text (In_channel.read_all fname) in
  let unfold_row row = row ^ "?" ^ row ^ "?" ^ row ^ "?" ^ row ^ "?" ^ row in
  let unfold_nums nums =
    let nums = List.of_array nums in
    nums @ nums @ nums @ nums @ nums |> Array.of_list
  in
  let input =
    List.map input ~f:(fun (row, nums) -> (unfold_row row, unfold_nums nums))
  in

  List.fold input ~init:0 ~f:(fun acc (row, nums) -> acc + solve_line row nums)
  |> Int.to_string

let%expect_test "solve line" =
  let input =
    parse_text
      {|???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1|}
  in
  List.iter input ~f:(fun (row, nums) ->
      print_endline row;
      print_s [%sexp (solve_line row nums : int)];
      print_endline "-------");
  [%expect
    {|
    ???.###
    1
    -------
    .??..??...?##.
    4
    -------
    ?#?#?#?#?#?#?#?
    1
    -------
    ????.#...#...
    1
    -------
    ????.######..#####.
    4
    -------
    ?###????????
    10
    ------- |}];
  ()
