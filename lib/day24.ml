open Core

let epsilon = 0.000001
let day = 24
let year = 2023

type t = {
  x : float;
  y : float;
  z : float; [@warning "-69"]
  vx : float;
  vy : float;
  vz : float; [@warning "-69"]
}
[@@deriving sexp]

let parse_text text =
  let get_nums nums =
    let nums =
      String.split nums ~on:','
      |> List.map ~f:(fun s -> String.strip s |> Int.of_string |> Int.to_float)
    in
    (List.nth_exn nums 0, List.nth_exn nums 1, List.nth_exn nums 2)
  in
  String.strip text |> String.split_lines
  |> List.map ~f:(fun line ->
         let line = String.split line ~on:'@' in
         let x, y, z = get_nums (List.nth_exn line 0) in
         let vx, vy, vz = get_nums (List.nth_exn line 1) in
         { x; y; z; vx; vy; vz })

(* https://byjus.com/point-of-intersection-formula/ *)
let get_abc line =
  let open Float in
  let slope = line.vy / line.vx in
  let zero_x_time = -line.x / line.vx in
  let intercept = line.y + (line.vy * zero_x_time) in
  (-slope, 1., -intercept)

(* I'm assuming that there won't be any duplicate lines in the input *)
let get_future_2d_intersection t1 t2 =
  let open Float in
  if t1.vx = t2.vx && t1.vy = t2.vy then None
  else
    let (a1, b1, c1), (a2, b2, c2) = (get_abc t1, get_abc t2) in
    let x = ((b1 * c2) - (b2 * c1)) / ((a1 * b2) - (a2 * b1)) in
    let y = ((a2 * c1) - (a1 * c2)) / ((a1 * b2) - (a2 * b1)) in
    let get_cross_time t x = (x - t.x) / t.vx in
    if is_negative (get_cross_time t1 x) || is_negative (get_cross_time t2 x)
    then None
    else Some (x, y)

let inside_area (x, y) (min_val, max_val) =
  let open Float in
  x + epsilon > min_val
  && y + epsilon > min_val
  && x - epsilon < max_val
  && y - epsilon < max_val

let get_num_intersections_inside_area lines min_val max_val =
  List.count (List.cartesian_product lines lines) ~f:(fun (t1, t2) ->
      match get_future_2d_intersection t1 t2 with
      | None -> false
      | Some pt -> inside_area pt (min_val, max_val))
  / 2

let part1 fname =
  let min_val, max_val =
    if String.(fname = "test_input/24.in") then (7., 27.)
    else (200000000000000., 400000000000000.)
  in
  let lines = parse_text (In_channel.read_all fname) in
  get_num_intersections_inside_area lines min_val max_val |> Int.to_string

let part2 _ = "i cheated for this one"

let%expect_test "line intersections" =
  let a = { x = 19.; y = 13.; z = 0.; vx = -2.; vy = 1.; vz = 0. } in
  let b = { x = 18.; y = 19.; z = 0.; vx = -1.; vy = -1.; vz = 0. } in
  print_s [%sexp (get_future_2d_intersection a b : (float * float) option)];
  [%expect {| ((14.333333333333334 15.333333333333334)) |}];
  ()
