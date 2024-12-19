open! Core

let of_string s =
  match String.split_lines s with
  | patterns :: "" :: tl ->
      ( String.filter patterns ~f:(fun c -> not (Char.is_whitespace c))
        |> String.split ~on:',',
        tl )
  | lines -> raise_s [%message "bad input string lines" (lines : string list)]

let can_make patterns towel =
  let rec aux towel =
    if String.length towel = 0 then true
    else
      List.exists patterns ~f:(fun pattern ->
          match String.chop_prefix towel ~prefix:pattern with
          | Some towel -> aux towel
          | None -> false)
  in
  aux towel

let num_ways_to_make patterns towel =
  let memo = Hashtbl.create (module String) in
  let rec aux towel =
    if String.length towel = 0 then 1
    else
      match Hashtbl.find memo towel with
      | Some res -> res
      | None ->
          let res =
            List.sum
              (module Int)
              patterns
              ~f:(fun pattern ->
                match String.chop_prefix towel ~prefix:pattern with
                | Some towel -> aux towel
                | None -> 0)
          in
          Hashtbl.add_exn memo ~key:towel ~data:res;
          res
  in
  aux towel

let part1 s =
  let patterns, towels = of_string s in
  List.count towels ~f:(fun towel -> can_make patterns towel)
  |> Int.to_string |> Ok

let part2 s =
  let patterns, towels = of_string s in
  List.sum (module Int) towels ~f:(fun towel -> num_ways_to_make patterns towel)
  |> Int.to_string |> Ok
