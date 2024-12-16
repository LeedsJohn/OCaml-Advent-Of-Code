open! Core
open! Helpers

(* I originally wrote this functionally but then I rewrote it with just a for loop and
   that brought it from ~100ms to ~1 ms for both parts *)

(* too lazy to install a regex library but that would make this cleaner i think *)
[@@@warning "-32"]

let get_numbers_in_parens s =
  let n1, s =
    if Char.is_digit (String.get s 0) then Parse.take_int s else (0, s)
  in
  let n1, s =
    if String.length s > 0 && Char.equal (String.get s 0) ',' then
      (n1, String.drop_prefix s 1)
    else (0, s)
  in
  let n2, s =
    if Char.is_digit (String.get s 0) then Parse.take_int s else (0, s)
  in
  let n2 =
    if String.length s > 0 && Char.equal (String.get s 0) ')' then n2 else 0
  in
  n1 * n2

let get_numbers_in_parens' s start_num1 =
  let end_num1 = ref start_num1 in
  while !end_num1 < String.length s && Char.is_digit (String.get s !end_num1) do
    end_num1 := !end_num1 + 1
  done;
  if
    start_num1 = !end_num1
    || !end_num1 >= String.length s
    || not (Char.equal (String.get s !end_num1) ',')
  then (0, !end_num1)
  else
    let end_num2 = ref (!end_num1 + 1) in
    while
      !end_num2 < String.length s && Char.is_digit (String.get s !end_num2)
    do
      end_num2 := !end_num2 + 1
    done;
    if
      !end_num1 + 1 = !end_num2
      || !end_num2 >= String.length s
      || not (Char.equal (String.get s !end_num2) ')')
    then (0, !end_num2)
    else
      let n1 = Int.of_string (String.slice s start_num1 !end_num1) in
      let n2 = Int.of_string (String.slice s (!end_num1 + 1) !end_num2) in
      (n1 * n2, !end_num2 + 1)

let part1 s =
  (* let rec aux acc s =
       if String.length s < 8 then acc
       else
         match String.prefix s 4 with
         | "mul(" ->
             aux
               (acc + get_numbers_in_parens (String.slice s 4 0))
               (String.drop_prefix s 4)
         | _ -> aux acc (String.drop_prefix s 1)
     in
     aux 0 s |> Int.to_string |> Ok *)
  let res = ref 0 in
  let i = ref 0 in
  while !i + 7 < String.length s do
    if String.equal (String.slice s !i (!i + 4)) "mul(" then (
      let num, new_i = get_numbers_in_parens' s (!i + 4) in
      res := !res + num;
      i := new_i)
    else i := !i + 1
  done;
  Int.to_string !res |> Ok

let part2 s =
  (* let instruction_type s =
       let open String in
       if prefix s 4 = "mul(" then `Mul
       else if prefix s 4 = "do()" then `Do
       else if prefix s 7 = "don't()" then `Don't
       else `John
     in
     let rec aux acc enabled s =
       if String.length s < 8 then acc
       else
         match instruction_type s with
         | `Mul ->
             if not enabled then aux acc enabled (String.drop_prefix s 4)
             else
               aux
                 (acc + get_numbers_in_parens (String.slice s 4 0))
                 true (String.drop_prefix s 4)
         | `Do -> aux acc true (String.drop_prefix s 1)
         | `Don't -> aux acc false (String.drop_prefix s 1)
         | `John -> aux acc enabled (String.drop_prefix s 1)
     in
     aux 0 true s |> Int.to_string |> Ok *)
  let res = ref 0 in
  let i = ref 0 in
  let good = ref true in
  while !i + 7 < String.length s do
    if !good then
      if String.equal (String.slice s !i (!i + 4)) "mul(" (* ) *) then (
        let num, new_i = get_numbers_in_parens' s (!i + 4) in
        res := !res + num;
        i := new_i)
      else if String.equal (String.slice s !i (!i + 7)) "don't()" then (
        i := !i + 7;
        good := false)
      else i := !i + 1
    else if String.equal (String.slice s !i (!i + 4)) "do()" then (
      i := !i + 4;
      good := true)
    else i := !i + 1
  done;
  Int.to_string !res |> Ok
