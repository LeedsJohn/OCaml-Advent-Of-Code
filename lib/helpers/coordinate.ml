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

let neighbors (x, y) =
  Set.of_list (module T) [ (x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1) ]

let rotate_right (x, y) = (-y, x)
let rotate_left t = rotate_right t |> rotate_right |> rotate_right
let turn_around t = rotate_right t |> rotate_right
let offsets = neighbors (0, 0)

let neighbors8 (x, y) =
  Set.of_list
    (module T)
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
  Set.of_list
    (module T)
    [ (x + 1, y + 1); (x + 1, y - 1); (x - 1, y + 1); (x - 1, y - 1) ]

let diagonal_offsets = diagonals (0, 0)
let add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
let scale (x, y) n = (x * n, y * n)

let shoot_ray ~start ~dir ~length =
  List.map (List.range 0 length) ~f:(fun i -> scale dir i |> add start)

module Direction4 = struct
  type t = Up | Down | Right | Left [@@deriving sexp, compare, equal, hash]

  let to_offset = function
    | Up -> (0, -1)
    | Down -> (0, 1)
    | Left -> (-1, 0)
    | Right -> (1, 0)
end
