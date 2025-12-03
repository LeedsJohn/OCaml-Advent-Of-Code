open! Core

type reg = char [@@deriving sexp_of]

module Instruction = struct
  type t =
    | Hlf of reg
    | Tpl of reg
    | Inc of reg
    | Jmp of int
    | Jie of reg * int
    | Jio of reg * int
  [@@deriving sexp_of]

  let of_string s =
    let ar = String.split s ~on:' ' |> List.to_array in
    match ar.(0) with
    | "hlf" -> Hlf (Char.of_string ar.(1))
    | "tpl" -> Tpl (Char.of_string ar.(1))
    | "inc" -> Inc (Char.of_string ar.(1))
    | "jmp" -> Jmp (Int.of_string ar.(1))
    | "jie" -> Jie (String.get ar.(1) 0, Int.of_string ar.(2))
    | "jio" -> Jio (String.get ar.(1) 0, Int.of_string ar.(2))
    | s -> raise_s [%message "invalid instruction" ~instruction:(s : string)]
  ;;
end

module State = struct
  type t =
    { a : int
    ; b : int
    ; ip : int
    ; instructions : Instruction.t array
    }

  let of_string ?(a = 0) s =
    let instructions =
      String.split_lines s |> List.map ~f:Instruction.of_string |> List.to_array
    in
    { a; b = 0; ip = 0; instructions }
  ;;

  let step ({ a; b; ip; instructions } as t) =
    match instructions.(ip) with
    | Hlf 'a' -> { t with a = a / 2; ip = ip + 1 }
    | Hlf 'b' -> { t with b = b / 2; ip = ip + 1 }
    | Tpl 'a' -> { t with a = a * 3; ip = ip + 1 }
    | Tpl 'b' -> { t with b = b * 3; ip = ip + 1 }
    | Inc 'a' -> { t with a = a + 1; ip = ip + 1 }
    | Inc 'b' -> { t with b = b + 1; ip = ip + 1 }
    | Jmp n -> { t with ip = ip + n }
    | Jie ('a', n) -> if a % 2 = 0 then { t with ip = ip + n } else { t with ip = ip + 1 }
    | Jie ('b', n) -> if b % 2 = 0 then { t with ip = ip + n } else { t with ip = ip + 1 }
    | Jio ('a', n) -> if a = 1 then { t with ip = ip + n } else { t with ip = ip + 1 }
    | Jio ('b', n) -> if b = 1 then { t with ip = ip + n } else { t with ip = ip + 1 }
    | instruction -> raise_s [%message "bad instruction" (instruction : Instruction.t)]
  ;;

  let rec run ({ ip; instructions; _ } as t) =
    if ip >= Array.length instructions then t else run (step t)
  ;;
end

let part1 s =
  let t = State.of_string s in
  let { b; _ } : State.t = State.run t in
  Int.to_string b
;;

let part2 s =
  let t = State.of_string ~a:1 s in
  let { b; _ } : State.t = State.run t in
  Int.to_string b
;;
