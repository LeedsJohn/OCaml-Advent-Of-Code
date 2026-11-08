open! Core

let parse s =
  let res = Hashtbl.create (module String) in
  String.split_lines s
  |> List.iter ~f:(fun line ->
    let pos = String.slice line 0 3 in
    let neighbors = String.slice line 5 0 |> String.split ~on:' ' in
    Hashtbl.set res ~key:pos ~data:neighbors);
  res
;;

let num_paths t =
  let cache = Hashtbl.create (module String) in
  let rec aux cur =
    if String.equal cur "out"
    then 1
    else
      Hashtbl.find_or_add cache cur ~default:(fun () ->
        let neighbors = Hashtbl.find_exn t cur in
        List.sum (module Int) neighbors ~f:aux)
  in
  aux "you"
;;

(* i'm assuming this shit acyclic *)
let num_paths2 t =
  let cache = Hashtbl.create (module String) in
  let rec aux passed_through cur =
    if String.equal cur "out"
    then Bool.to_int (passed_through = 3)
    else (
      let passed_through =
        passed_through
        + if String.equal cur "dac" then 1 else if String.equal cur "fft" then 2 else 0
      in
      Hashtbl.find_or_add
        cache
        [%string "%{cur}%{passed_through#Int}"]
        ~default:(fun () ->
          let neighbors = Hashtbl.find_exn t cur in
          List.sum (module Int) neighbors ~f:(aux passed_through)))
  in
  aux 0 "svr"
;;

let part1 s = parse s |> num_paths |> Int.to_string
let part2 s = parse s |> num_paths2 |> Int.to_string
