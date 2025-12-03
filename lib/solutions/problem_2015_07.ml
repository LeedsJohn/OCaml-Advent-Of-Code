open! Core

let mask = Int.shift_left 1 17 - 1

type wire = string [@@deriving sexp]

type arg =
  | Con of int
  | Wire of string
[@@deriving sexp]

type gate =
  | Noop of arg * wire
  | And of arg * arg * wire
  | Or of arg * arg * wire
  | Lshift of arg * arg * wire
  | Rshift of arg * arg * wire
  | Not of arg * wire
[@@deriving sexp]

let get_dest = function
  | Noop (_, r) -> r
  | And (_, _, r) -> r
  | Or (_, _, r) -> r
  | Lshift (_, _, r) -> r
  | Rshift (_, _, r) -> r
  | Not (_, r) -> r
;;

let get_gates s =
  let get_arg s =
    match Int.of_string_opt s with
    | None -> Wire s
    | Some n -> Con n
  in
  String.split_lines s
  |> List.map ~f:(fun line ->
    let stuff = String.split line ~on:' ' |> List.to_array in
    match stuff.(1) with
    | "AND" -> And (get_arg stuff.(0), get_arg stuff.(2), stuff.(4))
    | "OR" -> Or (get_arg stuff.(0), get_arg stuff.(2), stuff.(4))
    | "LSHIFT" -> Lshift (get_arg stuff.(0), get_arg stuff.(2), stuff.(4))
    | "RSHIFT" -> Rshift (get_arg stuff.(0), get_arg stuff.(2), stuff.(4))
    | _ ->
      if String.equal stuff.(0) "NOT"
      then Not (get_arg stuff.(1), stuff.(3))
      else Noop (get_arg stuff.(0), stuff.(2)))
;;

let gate_connected_to gates wire =
  List.find_exn gates ~f:(fun gate -> String.equal (get_dest gate) wire)
;;

let get_wire_val gates wire =
  let memo = Hashtbl.create (module String) in
  let rec aux wire : int =
    match wire with
    | Con n -> n
    | Wire wire ->
      (match Hashtbl.find memo wire with
       | Some res -> res
       | None ->
         let res =
           match gate_connected_to gates wire with
           | Noop (wire, _) -> aux wire
           | And (op1, op2, _) -> Int.bit_and (aux op1) (aux op2)
           | Or (op1, op2, _) -> Int.bit_or (aux op1) (aux op2)
           | Lshift (op1, op2, _) -> Int.bit_and (Int.shift_left (aux op1) (aux op2)) mask
           | Rshift (op1, op2, _) ->
             Int.bit_and (Int.shift_right (aux op1) (aux op2)) mask
           | Not (op, _) -> Int.bit_not (aux op)
         in
         Hashtbl.add_exn memo ~key:wire ~data:res;
         res)
  in
  aux wire
;;

let part1 s =
  let gates = get_gates s in
  get_wire_val gates (Wire "a") |> Int.to_string
;;

let part2 s =
  let gates = get_gates s in
  let a_val = get_wire_val gates (Wire "a") in
  let gates =
    Noop (Con a_val, "b")
    :: List.filter gates ~f:(fun gate -> not (String.equal (get_dest gate) "b"))
  in
  get_wire_val gates (Wire "a") |> Int.to_string
;;
