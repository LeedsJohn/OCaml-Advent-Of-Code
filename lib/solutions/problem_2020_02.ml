open! Core

module Entry = struct
  type t = {
    min_entries : int;
    max_entries : int;
    character : char;
    password : string;
  }
  [@@deriving sexp]

  let of_string s =
    let rec get_num acc s =
      let c = List.hd_exn s in
      if Char.is_digit c then get_num (acc ^ Char.to_string c) (List.tl_exn s)
      else (Int.of_string acc, List.tl_exn s)
    in
    let s = String.to_list s in
    let min_entries, s = get_num "" s in
    let max_entries, s = get_num "" s in
    let character = List.hd_exn s in
    let password = List.split_n s 3 |> snd |> String.of_list in
    { min_entries; max_entries; character; password }

  let is_valid t =
    let count = String.count t.password ~f:(Char.equal t.character) in
    Int.between count ~low:t.min_entries ~high:t.max_entries

  let is_valid2 { min_entries; max_entries; character; password } =
    let thing1 = Char.equal character (String.get password (min_entries - 1)) in
    let thing2 = Char.equal character (String.get password (max_entries - 1)) in
    Bool.(thing1 <> thing2)
end

let parse_input s = String.split_lines s |> List.map ~f:Entry.of_string

let part1 s =
  let input = parse_input s in
  Ok (List.count input ~f:Entry.is_valid |> Int.to_string)

let part2 s =
  let input = parse_input s in
  Ok (List.count input ~f:Entry.is_valid2 |> Int.to_string)
