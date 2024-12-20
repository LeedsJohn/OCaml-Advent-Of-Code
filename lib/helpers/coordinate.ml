open! Core

module T = struct
  module U = struct
    type t = int * int [@@deriving compare, sexp, equal, hash]
  end

  include U
  include Comparator.Make (U)
end

include T

type coordinate = t

let distance (x1, y1) (x2, y2) = Int.abs (x1 - x2) + Int.abs (y1 - y2)
let neighbors (x, y) = [ (x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1) ]
let rotate_right (x, y) = (-y, x)
let rotate_left t = rotate_right t |> rotate_right |> rotate_right
let turn_around t = rotate_right t |> rotate_right
let offsets = neighbors (0, 0)

let neighbors8 (x, y) =
  [
    (x + 1, y);
    (x - 1, y);
    (x, y + 1);
    (x, y - 1);
    (x + 1, y + 1);
    (x + 1, y - 1);
    (x - 1, y + 1);
    (x - 1, y - 1);
  ]

let offsets8 = neighbors8 (0, 0)

let diagonals (x, y) =
  [ (x + 1, y + 1); (x + 1, y - 1); (x - 1, y + 1); (x - 1, y - 1) ]

let diagonal_offsets = diagonals (0, 0)
let add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
let sub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

let scale_down (x, y) =
  let rec gcd a b =
    if a = b then a else if a > b then gcd (a - b) b else gcd a (b - a)
  in
  if x = 0 then (0, if y < 0 then -1 else if y = 0 then 0 else 1)
  else if y = 0 then ((if x < 0 then -1 else if x = 0 then 0 else 1), 0)
  else
    let d = gcd (Int.abs x) (Int.abs y) in
    (x / d, y / d)

let scale (x, y) n = (x * n, y * n)

let shoot_ray ~start ~dir ~length =
  List.map (List.range 0 length) ~f:(fun i -> scale dir i |> add start)
