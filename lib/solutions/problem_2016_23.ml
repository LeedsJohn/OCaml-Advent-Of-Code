(* it would be fun to rewrite this more nicely. and make it faster. *)
open! Core

module Operand = struct
  type t =
    | Literal of int
    | Reg of char
end

module Instruction = struct
  type t =
    | Cpy of (Operand.t * char)
    | Inc of char
    | Dec of char
    | Jnz of (Operand.t * Operand.t)
    | Tgl of Operand.t

  let toggle = function
    | Jnz (_, Literal _) -> None
    | Cpy (op, c) -> Some (Jnz (op, Reg c))
    | Jnz (op, Reg c) -> Some (Cpy (op, c))
    | Inc c -> Some (Dec c)
    | Dec c -> Some (Inc c)
    | Tgl (Literal _) -> None
    | Tgl (Reg c) -> Some (Inc c)
  ;;

  let of_string s =
    let ar = String.split s ~on:' ' |> List.to_array in
    let get_op s =
      match Int.of_string_opt s with
      | Some n -> Operand.Literal n
      | None -> Reg (String.get s 0)
    in
    match ar.(0) with
    | "inc" -> Inc (String.get ar.(1) 0)
    | "dec" -> Dec (String.get ar.(1) 0)
    | "jnz" -> Jnz (get_op ar.(1), get_op ar.(2))
    | "cpy" -> Cpy (get_op ar.(1), String.get ar.(2) 0)
    | "tgl" -> Tgl (get_op ar.(1))
    | s -> raise_s [%message "bad instruction" ~instruction:(s : string)]
  ;;
end

module Computer = struct
  type t =
    { a : int
    ; b : int
    ; c : int
    ; d : int
    ; ip : int
    ; instructions : Instruction.t option Map.M(Int).t
    }

  let start =
    { a = 0; b = 0; c = 0; d = 0; ip = 0; instructions = Map.empty (module Int) }
  ;;

  let get_a { a; _ } = a
  let get_b { b; _ } = b
  let get_c { c; _ } = c
  let get_d { d; _ } = d
  let set_a t a = { t with a }
  let set_b t b = { t with b }
  let set_c t c = { t with c }
  let set_d t d = { t with d }

  let get t c =
    match c with
    | 'a' -> get_a t
    | 'b' -> get_b t
    | 'c' -> get_c t
    | 'd' -> get_d t
    | reg -> raise_s [%message "bad register" (reg : char)]
  ;;

  let set t c thing =
    match c with
    | 'a' -> set_a t thing
    | 'b' -> set_b t thing
    | 'c' -> set_c t thing
    | 'd' -> set_d t thing
    | reg -> raise_s [%message "bad register" (reg : char)]
  ;;

  let get_val t : Operand.t -> int = function
    | Literal n -> n
    | Reg x -> get t x
  ;;

  let cpy t x y =
    let new_val = get_val t x in
    let t = set t y new_val in
    { t with ip = t.ip + 1 }
  ;;

  let inc t x =
    let t = set t x (get t x + 1) in
    { t with ip = t.ip + 1 }
  ;;

  let dec t x =
    let t = set t x (get t x - 1) in
    { t with ip = t.ip + 1 }
  ;;

  let jnz t (x : Operand.t) (y : Operand.t) =
    let x_val = get_val t x in
    let y_val = get_val t y in
    if x_val = 0 then { t with ip = t.ip + 1 } else { t with ip = t.ip + y_val }
  ;;

  let tgl t op =
    let op_val = get_val t op in
    let target = t.ip + op_val in
    let t = { t with ip = t.ip + 1 } in
    let instructions =
      Map.change t.instructions target ~f:(function
        | None -> None
        | Some None -> Some None
        | Some (Some instruction) -> Some (Instruction.toggle instruction))
    in
    { t with instructions }
  ;;

  let apply_instruction t : Instruction.t option -> t = function
    | None -> t
    | Some ins ->
      (match ins with
       | Cpy (x, y) -> cpy t x y
       | Inc x -> inc t x
       | Dec x -> dec t x
       | Jnz (x, y) -> jnz t x y
       | Tgl op -> tgl t op)
  ;;

  let run t =
    let rec aux ({ ip; _ } as t) =
      if Int.between ip ~low:0 ~high:(Map.length t.instructions - 1)
      then aux (apply_instruction t (Map.find_exn t.instructions ip))
      else t
    in
    aux t
  ;;
end

let get_instructions s =
  String.split_lines s
  |> List.to_array
  |> Array.map ~f:Instruction.of_string
  |> Array.map ~f:Option.some
  |> Array.to_list
  |> List.mapi ~f:(fun i instruction -> i, instruction)
  |> Map.of_alist_exn (module Int)
;;

let part1 s =
  let instructions = get_instructions s in
  let ({ a; _ } : Computer.t) =
    Computer.run { Computer.start with instructions; a = 7 }
  in
  Int.to_string a
;;

let part2 s =
  let instructions = get_instructions s in
  let ({ a; _ } : Computer.t) =
    Computer.run { Computer.start with instructions; a = 12 }
  in
  Int.to_string a
;;
