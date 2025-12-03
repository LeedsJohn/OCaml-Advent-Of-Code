open! Core
open! Helpers

module Wire = struct
  type t = int [@@deriving sexp]

  let mask = Int.shift_left 1 8 - 1

  let of_string w =
    let a = Char.to_int (String.get w 0) in
    let b = Int.shift_left (Char.to_int (String.get w 1)) 8 in
    let c = Int.shift_left (Char.to_int (String.get w 2)) 16 in
    Int.bit_or a b |> Int.bit_or c
  ;;

  let to_string n =
    let a = Char.of_int_exn (Int.bit_and n mask) in
    let n = Int.shift_right n 8 in
    let b = Char.of_int_exn (Int.bit_and n mask) in
    let n = Int.shift_right n 8 in
    let c = Char.of_int_exn (Int.bit_and n mask) in
    String.of_list [ a; b; c ]
  ;;

  let starts_with t c =
    let first_char = Char.of_int_exn (Int.bit_and t mask) in
    Char.( = ) c first_char
  ;;

  let is_input t = starts_with t 'x' || starts_with t 'y'
end

type bit_func =
  | Xor
  | And
  | Or
[@@deriving sexp]

let bf_equal b1 b2 =
  match b1, b2 with
  | Xor, Xor -> true
  | And, And -> true
  | Or, Or -> true
  | _, _ -> false
;;

let get_f = function
  | Xor -> Bool.( <> )
  | And -> fun b1 b2 -> b1 && b2
  | Or -> fun b1 b2 -> b1 || b2
;;

module System = struct
  type t =
    { wires : bool Hashtbl.M(Int).t
    ; gates : (Wire.t * Wire.t * Wire.t * bit_func) list
    ; num_inputs : int
    }
  [@@deriving sexp_of]

  let rec get ({ wires; gates; _ } as t) wire =
    match Hashtbl.find wires wire with
    | Some v -> v
    | None ->
      if Wire.is_input wire
      then false
      else (
        let in1, in2, _, f = List.find_exn gates ~f:(fun (_, _, out, _) -> out = wire) in
        let in1 = get t in1 in
        let in2 = get t in2 in
        let res = (get_f f) in1 in2 in
        Hashtbl.add_exn wires ~key:wire ~data:res;
        res)
  ;;

  let get_all_wires { gates; _ } =
    List.fold
      gates
      ~init:(Set.empty (module Int))
      ~f:(fun acc (in1, in2, out, _) ->
        Set.union acc (Set.of_list (module Int) [ in1; in2; out ]))
    |> Set.to_list
  ;;

  let get_wires_starting_with t x =
    get_all_wires t |> List.filter ~f:(fun wire -> Wire.starts_with wire x)
  ;;

  let make wire_initial_values gates num_inputs =
    let wires = Hashtbl.create (module Int) in
    List.iter wire_initial_values ~f:(fun (wire, data) ->
      if data then Hashtbl.add_exn wires ~key:wire ~data:true);
    { wires; gates; num_inputs }
  ;;

  let of_string s =
    let lines = String.split_lines s in
    let initial_values, gates =
      List.filter lines ~f:(fun line -> String.length line > 1)
      |> List.partition_tf ~f:(fun line -> String.mem line ':')
    in
    let max_input_index = ref 0 in
    let initial_values =
      List.map initial_values ~f:(fun line ->
        max_input_index
        := Int.max !max_input_index (Int.of_string (String.slice line 1 3));
        let wire = String.slice line 0 3 |> Wire.of_string in
        let data = Char.equal (String.get line (String.length line - 1)) '1' in
        wire, data)
    in
    let gates =
      List.map gates ~f:(fun gate ->
        let inp1 = String.slice gate 0 3 |> Wire.of_string in
        let f, gate =
          match String.get gate 4 with
          | 'X' -> Xor, String.slice gate 8 0
          | 'O' -> Or, String.slice gate 7 0
          | 'A' -> And, String.slice gate 8 0
          | c -> raise_s [%message "Bad operator character" (c : char)]
        in
        let inp2 = String.slice gate 0 3 |> Wire.of_string in
        let out = String.slice gate 7 10 |> Wire.of_string in
        inp1, inp2, out, f)
    in
    make initial_values gates (!max_input_index + 1)
  ;;

  let section_binary t x =
    get_wires_starting_with t x
    |> List.map ~f:Wire.to_string
    |> List.sort ~compare:String.compare
    |> List.map ~f:Wire.of_string
    |> List.map ~f:(get t)
    |> List.map ~f:(fun b -> if b then '1' else '0')
    |> String.of_list
  ;;

  let section_decimal t x = Numbers.bin_string_to_int (section_binary t x)

  let two_digit_num n =
    let s = Int.to_string n in
    if String.length s = 1 then "0" ^ s else s
  ;;

  let get_matching_node { gates; _ } i f =
    let x = Wire.of_string ("x" ^ two_digit_num i) in
    let y = Wire.of_string ("y" ^ two_digit_num i) in
    List.find_map gates ~f:(fun (in1, in2, out, f2) ->
      if Int.min x y = Int.min in1 in2 && Int.max x y = Int.max in1 in2 && bf_equal f f2
      then Some out
      else None)
  ;;

  let get_xor_node t i = get_matching_node t i Xor
  let get_and_node t i = get_matching_node t i And
  let memo = Hashtbl.create (module Int)

  let rec get_carrynode1 ({ gates; _ } as t) i =
    let%bind.Option xor_node = get_xor_node t i in
    let%bind.Option prev_carry = get_carrynode t (i - 1) in
    List.find_map gates ~f:(fun (in1, in2, out, f) ->
      if
        Int.min xor_node prev_carry = Int.min in1 in2
        && Int.max xor_node prev_carry = Int.max in1 in2
        && bf_equal f And
      then Some out
      else None)

  and get_carrynode ({ gates; _ } as t) i =
    match Hashtbl.find memo i with
    | Some res -> res
    | None ->
      let res =
        if i = 0
        then Wire.of_string "gct" |> Some
        else (
          let%bind.Option prev_carry1 = get_carrynode1 t i in
          let%bind.Option and_node = get_and_node t i in
          List.find_map gates ~f:(fun (in1, in2, out, f) ->
            if
              Int.min and_node prev_carry1 = Int.min in1 in2
              && Int.max and_node prev_carry1 = Int.max in1 in2
              && bf_equal f Or
            then Some out
            else None))
      in
      Hashtbl.add_exn memo ~key:i ~data:res;
      res
  ;;
end

let part1 s =
  let system = System.of_string s in
  System.section_decimal system 'z' |> Int.to_string
;;

let part2 s =
  let system = System.of_string s in
  let i = ref 0 in
  while !i < 45 do
    print_s
      [%message
        ""
          ~i:(!i : int)
          ~carrynode:
            (System.get_carrynode system !i |> Option.map ~f:Wire.to_string
             : string option)];
    i := !i + 1
  done;
  "i did this one with manual inspection"
;;
