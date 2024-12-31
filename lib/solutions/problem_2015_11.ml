open! Core

let ctn c = Char.to_int c - Char.to_int 'a'
let ntc n = Char.of_int_exn (n + Char.to_int 'a')
let get t i = t / Int.pow 26 i % 26
let set t i c = (ctn c * Int.pow 26 i) + t

let to_string t =
  List.map (List.range 0 8) ~f:(get t)
  |> List.rev |> List.map ~f:ntc |> String.of_list

let of_string s =
  print_endline s;
  String.foldi (String.rev s) ~init:0 ~f:(fun i acc c -> set acc i c)

let has_increasing t =
  let i = ref 0 in
  let res = ref false in
  while !i <= 5 do
    let n1 = get t !i in
    let n2 = get t (!i + 1) in
    let n3 = get t (!i + 2) in
    if n1 - 1 = n2 && n2 - 1 = n3 then (
      res := true;
      i := 6)
    else i := !i + 1
  done;
  !res

let no_confusing_chars t =
  let i = ref 0 in
  let res = ref true in
  while !i <= 7 do
    if String.mem "iol" (ntc (get t !i)) then (
      res := false;
      i := 100)
    else i := !i + 1
  done;
  !res

let has_doubles t =
  let i = ref 0 in
  let count = ref 0 in
  while !i <= 6 do
    let n1 = get t !i in
    let n2 = get t (!i + 1) in
    if n1 = n2 then (
      count := !count + 1;
      i := !i + 2)
    else i := !i + 1
  done;
  !count >= 2

let good_password t = has_increasing t && no_confusing_chars t && has_doubles t

let part1 s =
  let t = ref (of_string s) in
  while not (good_password !t) do
    t := !t + 1
  done;
  to_string !t |> Ok

let part2 s =
  let t = ref (of_string s) in
  let count = ref 0 in
  while !count < 2 do
    if good_password !t then count := !count + 1;
    t := !t + 1
  done;
  to_string (!t - 1) |> Ok

let%expect_test "" =
  let t = of_string "aaaaaaay" in
  print_s [%sexp (to_string t : string)];
  [%expect {|
    aaaaaaay
    aaaaaaay
    |}];
  let t = of_string "aaaaaaay" in
  print_s [%sexp (to_string (t + 1) : string)];
  [%expect {|
    aaaaaaay
    aaaaaaaz
    |}];
  print_s [%sexp (to_string (t + 2) : string)];
  [%expect {| aaaaaaba |}];
  ()
