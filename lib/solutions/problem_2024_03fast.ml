open! Core

let get_numbers_in_parens s start_num1 =
  let end_num1 = ref start_num1 in
  while !end_num1 < String.length s && Char.is_digit (String.get s !end_num1) do
    end_num1 := !end_num1 + 1
  done;
  if
    start_num1 = !end_num1
    || !end_num1 >= String.length s
    || not (Char.equal (String.get s !end_num1) ',')
  then 0, !end_num1
  else (
    let end_num2 = ref (!end_num1 + 1) in
    while !end_num2 < String.length s && Char.is_digit (String.get s !end_num2) do
      end_num2 := !end_num2 + 1
    done;
    if
      !end_num1 + 1 = !end_num2
      || !end_num2 >= String.length s
      || not (Char.equal (String.get s !end_num2) ')')
    then 0, !end_num2
    else (
      let n1 = Int.of_string (String.slice s start_num1 !end_num1) in
      let n2 = Int.of_string (String.slice s (!end_num1 + 1) !end_num2) in
      n1 * n2, !end_num2 + 1))
;;

let part1 s =
  let res = ref 0 in
  let i = ref 0 in
  while !i + 7 < String.length s do
    if String.equal (String.slice s !i (!i + 4)) "mul("
    then (
      let num, new_i = get_numbers_in_parens s (!i + 4) in
      res := !res + num;
      i := new_i)
    else i := !i + 1
  done;
  Int.to_string !res
;;

let part2 s =
  let res = ref 0 in
  let i = ref 0 in
  let good = ref true in
  while !i + 7 < String.length s do
    if !good
    then
      if String.equal (String.slice s !i (!i + 4)) "mul(" (* ) *)
      then (
        let num, new_i = get_numbers_in_parens s (!i + 4) in
        res := !res + num;
        i := new_i)
      else if String.equal (String.slice s !i (!i + 7)) "don't()"
      then (
        i := !i + 7;
        good := false)
      else i := !i + 1
    else if String.equal (String.slice s !i (!i + 4)) "do()"
    then (
      i := !i + 4;
      good := true)
    else i := !i + 1
  done;
  Int.to_string !res
;;
