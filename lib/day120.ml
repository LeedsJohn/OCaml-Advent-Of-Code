open! Core

let day = 120
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
         in
         (row, nums))

(* let get_row_segments row =
     let last_segment, res =
       List.fold row ~init:(0, []) ~f:(fun (num_prev, acc) c ->
           if Char.(c = '#') then (num_prev + 1, acc)
           else if num_prev <> 0 then (0, num_prev :: acc)
           else (0, acc))
     in
     let res = if last_segment = 0 then res else last_segment :: res in
     List.rev res

   let sum_nums nums = List.fold nums ~init:0 ~f:( + )

   let replace s i c =
     let n = String.length s in
     if i = 0 then c ^ String.slice s 1 n
     else if i + 1 = n then String.slice s 0 (n - 1) ^ c
     else String.slice s 0 i ^ c ^ String.slice s (i + 1) n

    let solve_line row nums =
      let count = sum_nums nums in
      let max_length = List.max_elt nums ~compare:Int.compare |> Option.value_exn in
      let rec aux row =
        let row_segments = get_row_segments (String.to_list row) in
        if sum_nums row_segments = count then
          Bool.to_int (List.equal (fun n1 n2 -> n1 = n2) row_segments nums)
        else if
          List.max_elt row_segments ~compare:Int.compare
          |> Option.value ~default:0 > max_length
        then 0
        else
          match String.findi row ~f:(fun _ c -> Char.(c = '?')) with
          | None -> 0
          | Some (i, _) ->
              let new_row1 = replace row i "#" in
              let new_row2 = replace row i "." in
              aux new_row1 + aux new_row2
      in
      aux row *)

module State = struct
  type t = char list * int list [@@deriving compare, hash, sexp_of]
end

let solve_line2 row nums =
  let row = String.to_list row in
  let start_with_segment_length row num =
    let l = List.length row in
    if l < num then false
    else
      let valid_start =
        List.for_all (List.take row num) ~f:(fun c -> Char.(c = '#' || c = '?'))
      in
      let valid_end =
        if l = num then true
        else
          match List.nth row num with
          | None -> false
          | Some c -> Char.(c = '?' || c = '.')
      in
      valid_start && valid_end
  in
  let visited = Hashtbl.create (module State) in
  let rec aux row nums =
    let row = List.drop_while row ~f:(fun c -> Char.(c = '.')) in
    match Hashtbl.find visited (row, nums) with
    | Some n -> n
    | None ->
        let res =
          if
            List.length row
            < List.fold nums ~init:(List.length nums - 1) ~f:( + )
          then 0
          else
            match (List.is_empty row, List.is_empty nums) with
            | true, true -> 1
            | true, false -> 0
            | false, true ->
                List.for_all row ~f:(fun c -> Char.(c = '?' || c = '.'))
                |> Bool.to_int
            | false, false ->
                let skip_question =
                  if Char.(List.hd_exn row = '?') then
                    aux (List.tl_exn row) nums
                  else 0
                in
                let first_seg_length = List.hd_exn nums in
                let replace_question =
                  if start_with_segment_length row first_seg_length then
                    let nums_tail = List.tl_exn nums in
                    let dropped_row = List.drop row (first_seg_length + 1) in
                    aux dropped_row nums_tail
                  else 0
                in
                skip_question + replace_question
        in
        Hashtbl.add_exn visited ~key:(row, nums) ~data:res;
        res
  in

  aux row nums

let part1 fname =
  let input = parse_text (In_channel.read_all fname) in
  List.fold input ~init:0 ~f:(fun acc (row, nums) -> acc + solve_line2 row nums)
  |> Int.to_string

let part2 fname =
  let input = parse_text (In_channel.read_all fname) in
  let unfold_row row = row ^ "?" ^ row ^ "?" ^ row ^ "?" ^ row ^ "?" ^ row in
  let unfold_nums nums = nums @ nums @ nums @ nums @ nums in
  let input =
    List.map input ~f:(fun (row, nums) -> (unfold_row row, unfold_nums nums))
  in

  List.fold input ~init:0 ~f:(fun acc (row, nums) -> acc + solve_line2 row nums)
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
      print_s [%sexp (solve_line2 row nums : int)];
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
