open! Core

let passport_lines s =
  String.substr_replace_all s ~pattern:"\n\n" ~with_:"*"
  |> String.split ~on:'*'
  |> List.map ~f:(String.substr_replace_all ~pattern:"\n" ~with_:" ")

let is_valid line =
  List.for_all [ "byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid" ]
    ~f:(fun field -> String.is_substring line ~substring:(field ^ ":"))

let get_field line field =
  let line = line ^ " " in
  let start = String.substr_index line ~pattern:(field ^ ":") in
  match start with
  | None -> None
  | Some start ->
      let end_ =
        String.findi line ~f:(fun i c -> i > start && Char.equal c ' ')
        |> Option.value_exn |> fst
      in
      Some (String.slice line (start + 4) end_)

let is_num_between s ~low ~high =
  let num = Int.of_string_opt s |> Option.value ~default:(low - 1) in
  Int.between num ~low ~high

let length_num_between s ~length ~low ~high =
  let num = Int.of_string_opt s |> Option.value ~default:(low - 1) in
  String.length s = length && Int.between num ~low ~high

let byr line =
  get_field line "byr" |> Option.value ~default:""
  |> length_num_between ~length:4 ~low:1920 ~high:2002

let iyr line =
  get_field line "iyr" |> Option.value ~default:""
  |> length_num_between ~length:4 ~low:2010 ~high:2020

let eyr line =
  get_field line "eyr" |> Option.value ~default:""
  |> length_num_between ~length:4 ~low:2020 ~high:2030

let hgt line =
  let s = get_field line "hgt" |> Option.value ~default:"" in
  if String.length s < 3 then false
  else
    let n = String.length s in
    match String.slice s (n - 2) n with
    | "cm" -> is_num_between (String.slice s 0 (n - 2)) ~low:150 ~high:193
    | "in" -> is_num_between (String.slice s 0 (n - 2)) ~low:59 ~high:76
    | _ -> false

let hcl line =
  let line = get_field line "hcl" |> Option.value ~default:"" in
  match String.length line with
  | 7 ->
      String.for_alli line ~f:(fun i c ->
          if i = 0 then Char.equal c '#'
          else String.contains "0123456789abcdef" c)
  | _ -> false

let ecl line =
  let s = get_field line "ecl" |> Option.value ~default:"" in
  String.length s = 3
  && String.is_substring "amb blu brn gry grn hzl oth " ~substring:(s ^ " ")

let pid line =
  let s = get_field line "pid" |> Option.value ~default:"" in
  String.length s = 9 && String.for_all s ~f:Char.is_digit

let is_valid2 line =
  List.for_all [ byr; iyr; eyr; hgt; hcl; ecl; pid ] ~f:(fun john -> john line)

let part1 s = Ok (passport_lines s |> List.count ~f:is_valid |> Int.to_string)
let part2 s = Ok (passport_lines s |> List.count ~f:is_valid2 |> Int.to_string)
