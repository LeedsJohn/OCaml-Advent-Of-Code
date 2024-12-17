open! Core
open! Helpers

module Comp = struct
  type t = {
    program : int Map.M(Int).t;
    ip : int;
    a : int;
    b : int;
    c : int;
    output : int Map.M(Int).t;
  }
  [@@deriving sexp_of]

  let of_string s =
    let a, s = Parse.take_next_int s in
    let b, s = Parse.take_next_int s in
    let c, s = Parse.take_next_int s in
    let i = String.substr_index_exn s ~pattern:"Program: " + 9 in
    let program =
      String.slice s i 0
      |> String.filter ~f:(fun c -> not (Char.equal ',' c))
      |> String.foldi
           ~init:(Map.empty (module Int))
           ~f:(fun i acc c ->
             Map.add_exn acc ~key:i ~data:(Char.get_digit_exn c))
    in
    { program; ip = 0; a; b; c; output = Map.empty (module Int) }

  let get_combo_operand { program; ip; a; b; c; _ } =
    match Map.find_exn program (ip + 1) with
    | (0 | 1 | 2 | 3) as n -> n
    | 4 -> a
    | 5 -> b
    | 6 -> c
    | n -> raise_s [%message "invalid combo operand" ~operand:(n : int)]

  let get_literal_operand { program; ip; _ } = Map.find_exn program (ip + 1)
  let funny_div n d = n / Int.pow 2 d

  let adv ({ a; ip; _ } as t) =
    let a = funny_div a (get_combo_operand t) in
    { t with a; ip = ip + 2 }

  let bxl ({ b; ip; _ } as t) =
    let b = Int.bit_xor b (get_literal_operand t) in
    { t with b; ip = ip + 2 }

  let bst ({ ip; _ } as t) = { t with b = get_combo_operand t % 8; ip = ip + 2 }

  let jnz ({ a; ip; _ } as t) =
    match a with
    | 0 -> { t with ip = ip + 2 }
    | _ -> { t with ip = get_literal_operand t }

  let bxc ({ b; c; ip; _ } as t) = { t with b = Int.bit_xor b c; ip = ip + 2 }

  let out ({ output; ip; _ } as t) =
    let output_num = get_combo_operand t % 8 in
    let output = Map.add_exn output ~key:(Map.length output) ~data:output_num in
    { t with output; ip = ip + 2 }

  let bdv ({ a; ip; _ } as t) =
    let b = funny_div a (get_combo_operand t) in
    { t with b; ip = ip + 2 }

  let cdv ({ a; ip; _ } as t) =
    let c = funny_div a (get_combo_operand t) in
    { t with c; ip = ip + 2 }

  let num_to_op = function
    | 0 -> adv
    | 1 -> bxl
    | 2 -> bst
    | 3 -> jnz
    | 4 -> bxc
    | 5 -> out
    | 6 -> bdv
    | 7 -> cdv
    | op -> raise_s [%message "invalid op number" (op : int)]

  let rec run ({ program; ip; _ } as t) =
    match Map.find program ip with None -> t | Some n -> run ((num_to_op n) t)

  let is_quine t a =
    let rec take_step ({ program; ip; output; _ } as t) =
      match Map.find program ip with
      | None -> Some t
      | Some 5 ->
          if Map.length output >= Map.length program then None
          else
            let new_t = out t in
            let i = Map.length new_t.output - 1 in
            if Map.find_exn new_t.output i <> Map.find_exn new_t.program i then
              None
            else take_step new_t
      | Some n -> take_step ((num_to_op n) t)
    in
    match take_step { t with a } with
    | None -> false
    | Some ran -> Map.equal Int.equal ran.output ran.program

  let output_string { output; _ } =
    let s =
      List.fold
        (List.range 0 (Map.length output))
        ~init:[]
        ~f:(fun acc n ->
          let c = Map.find_exn output n |> Int.to_string |> Char.of_string in
          c :: ',' :: acc)
      |> List.rev |> String.of_list
    in
    String.drop_prefix s 1
end

let part1 s =
  let comp = Comp.of_string s in
  let comp = Comp.run comp in
  Ok (Comp.output_string comp)

let part2 s =
  let comp = Comp.of_string s in
  let rec aux i =
      if (i % 10000000) = 0 then print_s [%sexp (i : int)];
      if Comp.is_quine comp i then i else aux (i + 1)
  in
  aux 0 |> Int.to_string |> Ok
