open! Core

let parse_stuff stuff =
  String.split stuff ~on:','
  |> List.map ~f:(String.split ~on:'-')
  |> List.map ~f:(function
       | [ a; b ] -> (Int.of_string a, Int.of_string b)
       | _ -> raise_s [%message "bruh"])

let john n =
  let s = Int.to_string n in
  if String.length s % 2 <> 0 then false
  else
    let half = String.length s / 2 in
    String.(prefix s half = suffix s half)

let john2 n =
  let s = Int.to_string n in
  let check_len n =
    let copies = String.length s / n in
    if copies * n <> String.length s then false
    else
      String.equal
        (List.init copies ~f:(fun _ -> String.prefix s n) |> String.concat)
        s
  in
  List.range 1 (String.length s) |> List.exists ~f:check_len

let part1 stuff =
  parse_stuff stuff
  |> List.sum
       (module Int)
       ~f:(fun (s, e) ->
         List.range s (e + 1)
         |> List.sum (module Int) ~f:(fun n -> if john n then n else 0))
  |> Int.to_string |> Ok

let part2 stuff =
  parse_stuff stuff
  |> List.sum
       (module Int)
       ~f:(fun (s, e) ->
         List.range s (e + 1)
         |> List.sum (module Int) ~f:(fun n -> if john2 n then n else 0))
  |> Int.to_string |> Ok
