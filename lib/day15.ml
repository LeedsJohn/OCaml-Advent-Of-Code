open Core

let day = 15
let year = 2023
let parse_text text = String.strip text |> String.split ~on:','

let hash text =
  String.fold text ~init:0 ~f:(fun acc c ->
      let n = Char.to_int c in
      (acc + n) * 17 % 256)

let part1 fname =
  let input = parse_text (In_channel.read_all fname) in
  List.map input ~f:hash |> List.fold ~init:0 ~f:( + ) |> Int.to_string

type token = Subtract of string | Add of string * int [@@deriving sexp]

let parse_text2 text =
  String.strip text |> String.split ~on:','
  |> List.map ~f:(fun s ->
         let len = String.length s in
         match String.index s '=' with
         | None -> Subtract (String.slice s 0 (len - 1))
         | Some i ->
             Add
               (String.slice s 0 i, String.slice s (i + 1) len |> Int.of_string))

let part2 fname =
  let lines = parse_text2 (In_channel.read_all fname) in
  let buckets = Array.create ~len:256 [] in
  List.iter lines ~f:(fun token ->
      match token with
      | Subtract s ->
          let i = hash s in
          buckets.(i) <-
            List.filter buckets.(i) ~f:(fun (label, _) -> String.(s <> label))
      | Add (s, focal_length) -> (
          let i = hash s in
          match
            List.find buckets.(i) ~f:(fun (label, _) -> String.(s = label))
          with
          | Some (_, n) -> n := focal_length
          | None -> buckets.(i) <- (s, ref focal_length) :: buckets.(i)));
  let buckets = Array.map buckets ~f:List.rev in
  Array.foldi buckets ~init:0 ~f:(fun i acc bucket ->
      List.foldi bucket ~init:acc ~f:(fun j acc (_, n) ->
          acc + ((i + 1) * (j + 1) * !n)))
  |> Int.to_string

let%expect_test "hash" =
  let lines =
    parse_text "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"
  in
  List.iter lines ~f:(fun line -> printf "%d\n" (hash line));
  [%expect
    {|
    30
    253
    97
    47
    14
    180
    9
    197
    48
    214
    231 |}];
  printf "%d" (List.fold (List.map lines ~f:hash) ~init:0 ~f:( + ));
  [%expect {| 1320 |}];
  ()
