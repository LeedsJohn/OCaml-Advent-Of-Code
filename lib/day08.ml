open! Core

let day = 8
let year = 2023

let parse_input lines =
  let lines = Array.of_list lines in
  let instructions = lines.(0) in
  let graph =
    Array.fold
      (Array.slice lines 2 (Array.length lines))
      ~init:(Map.empty (module String))
      ~f:(fun acc line ->
        let start = String.slice line 0 3 in
        let left = String.slice line 7 10 in
        let right = String.slice line 12 15 in
        Map.add_exn acc ~key:start ~data:(left, right))
  in
  (instructions, graph)

let get_next_node graph cur = function
  | 'L' -> Map.find_exn graph cur |> fst
  | _ -> Map.find_exn graph cur |> snd

let part1 fname =
  let instructions, graph = parse_input (In_channel.read_lines fname) in
  if List.exists (Map.keys graph) ~f:(String.equal "AAA") |> not then
    "bad input"
  else
    let rec aux position count =
      if String.equal position "ZZZ" then count
      else
        let direction =
          String.get instructions (count % String.length instructions)
        in
        aux (get_next_node graph position direction) (count + 1)
    in
    aux "AAA" 0 |> Int.to_string

let get_cycle_length graph instructions start_pos =
  print_endline start_pos;
  let rec aux pos count =
    if Char.equal (String.get pos 2) 'Z' then count
    else
      let direction =
        String.get instructions (count % String.length instructions)
      in
      aux (get_next_node graph pos direction) (count + 1)
  in
  aux start_pos 0

(* let rec aux pos count visited =
     match Map.find visited pos with
     | Some n -> count - n
     | None ->
         let direction =
           String.get instructions (count % String.length instructions)
         in
         aux
           (get_next_node graph pos direction)
           (count + 1)
           (Map.add_exn visited ~key:pos ~data:count)
   in
   aux start_pos 0 (Map.empty (module String)) *)

let rec gcd n1 n2 = if n2 = 0 then n1 else gcd n2 (n1 % n2)
let lcm n1 n2 = n1 / gcd n1 n2 * n2

let part2 fname =
  let instructions, graph = parse_input (In_channel.read_lines fname) in
  List.fold (Map.keys graph) ~init:1 ~f:(fun acc start_pos ->
      if Char.equal (String.get start_pos 2) 'A' |> not then acc
      else
        let cycle_length = get_cycle_length graph instructions start_pos in
        lcm acc cycle_length)
  |> Int.to_string

(* type end_positions = {
     end_positions : int list;
     start_cycle : int;
     end_cycle : int;
   }
   [@@deriving sexp]

   let get_end_positions graph instructions start_pos =
     let rec aux acc pos count visited =
       let k = pos ^ Int.to_string (count % String.length instructions) in
       match Map.find visited k with
       | Some n ->
           { end_positions = List.rev acc; start_cycle = n; end_cycle = count }
       | None ->
           let acc =
             if Char.equal (String.get pos 2) 'Z' then count :: acc else acc
           in
           let direction =
             String.get instructions (count % String.length instructions)
           in
           aux acc
             (get_next_node graph pos direction)
             (count + 1)
             (Map.add_exn visited ~key:k ~data:count)
     in
     aux [] start_pos 0 (Map.empty (module String))

   let end_at_step end_info step =
     List.exists end_info.end_positions ~f:(fun n ->
         if n = step then true
         else if step < end_info.start_cycle then false
         else
           let cycle_length = end_info.end_cycle - end_info.start_cycle in
           let step_in_cycle = (step - end_info.start_cycle) % cycle_length in
           end_info.start_cycle + step_in_cycle = n)

   let get_next_end_pos graph instructions pos start_count =
     let rec aux pos count =
       if start_count <> count && Char.equal (String.get pos 2) 'Z' then
         (pos, count)
       else
         let direction =
           String.get instructions (count % String.length instructions)
         in
         aux (get_next_node graph pos direction) (count + 1)
     in
     aux pos start_count

   let part2 fname =
     let instructions, graph = parse_input (In_channel.read_lines fname) in
     let end_information =
       List.fold (Map.keys graph) ~init:[] ~f:(fun acc pos ->
           if Char.equal (String.get pos 2) 'A' then
             get_end_positions graph instructions pos :: acc
           else acc)
     in
     let step_num = ref 0 in
     let cur_pos =
       ref
         (List.find_exn (Map.keys graph) ~f:(fun s ->
              Char.equal (String.get s 2) 'A'))
     in
     let res = ref None in
     let i = ref 0 in
     while Option.is_none !res do
       if !i % 10000 = 0 then print_endline (Int.to_string !step_num);
       i := !i + 1;
       if
         List.for_all end_information ~f:(fun end_info ->
             end_at_step end_info !step_num)
       then res := Some !step_num;
       let next_pos, updated_step_num =
         get_next_end_pos graph instructions !cur_pos !step_num
       in
       step_num := updated_step_num;
       cur_pos := next_pos
     done;
     Option.value_exn !res |> Int.to_string

   let%expect_test "stuff" =
     let instructions, graph =
       parse_input
         (String.split_lines
            {|LR

   11A = (11B, XXX)
   11B = (XXX, 11Z)
   11Z = (11B, XXX)
   22A = (22B, XXX)
   22B = (22C, 22C)
   22C = (22Z, 22Z)
   22Z = (22B, 22B)
   XXX = (XXX, XXX)|})
     in
     let stuff = get_end_positions graph instructions "11A" in
     print_s [%sexp (stuff : end_positions)];
     [%expect {| ((end_positions (2)) (start_cycle 1) (end_cycle 3)) |}];
     List.iter (List.range 0 10) ~f:(fun i ->
         print_endline
           ("Pos: " ^ Int.to_string i ^ " end: "
           ^ Bool.to_string (end_at_step stuff i)));
     [%expect
       {|
       Pos: 0 end: false
       Pos: 1 end: false
       Pos: 2 end: true
       Pos: 3 end: false
       Pos: 4 end: true
       Pos: 5 end: false
       Pos: 6 end: true
       Pos: 7 end: false
       Pos: 8 end: true
       Pos: 9 end: false |}];
     () *)
