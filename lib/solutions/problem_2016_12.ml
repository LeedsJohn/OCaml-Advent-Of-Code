open! Core

module Operand = struct
  type t = Literal of int | Reg of char
end

module Instruction = struct
  type t =
    | Cpy of Operand.t * char
    | Inc of char
    | Dec of char
    | Jnz of Operand.t * int

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
    | "jnz" -> Jnz (get_op ar.(1), Int.of_string ar.(2))
    | "cpy" -> Cpy (get_op ar.(1), String.get ar.(2) 0)
    | s -> raise_s [%message "bad instruction" ~instruction:(s : string)]
end

module Computer = struct
  type t = { a : int; b : int; c : int; d : int; ip : int }

  let start = { a = 0; b = 0; c = 0; d = 0; ip = 0 }
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

  let set t c thing =
    match c with
    | 'a' -> set_a t thing
    | 'b' -> set_b t thing
    | 'c' -> set_c t thing
    | 'd' -> set_d t thing
    | reg -> raise_s [%message "bad register" (reg : char)]

  let cpy t x y =
    let new_val = match x with Operand.Literal n -> n | Reg c -> get t c in
    let t = set t y new_val in
    { t with ip = t.ip + 1 }

  let inc t x =
    let t = set t x (get t x + 1) in
    { t with ip = t.ip + 1 }

  let dec t x =
    let t = set t x (get t x - 1) in
    { t with ip = t.ip + 1 }

  let jnz t x y =
    let x_val = match x with Operand.Literal n -> n | Reg x -> get t x in
    if x_val = 0 then { t with ip = t.ip + 1 } else { t with ip = t.ip + y }

  let apply_instruction t = function
    | Instruction.Cpy (x, y) -> cpy t x y
    | Inc x -> inc t x
    | Dec x -> dec t x
    | Jnz (x, y) -> jnz t x y

  let run t instructions =
    let rec aux ({ ip; _ } as t) =
      if Int.between ip ~low:0 ~high:(Array.length instructions - 1) then
        aux (apply_instruction t instructions.(ip))
      else t
    in
    aux t
end

let get_instructions s =
  String.split_lines s |> List.to_array |> Array.map ~f:Instruction.of_string

let part1 s =
  let ({ a; _ } : Computer.t) =
    Computer.run Computer.start (get_instructions s)
  in
  Int.to_string a |> Ok

let part2 s =
  let start_computer = { Computer.start with c = 1 } in
  let ({ a; _ } : Computer.t) =
    Computer.run start_computer (get_instructions s)
  in
  Int.to_string a |> Ok
