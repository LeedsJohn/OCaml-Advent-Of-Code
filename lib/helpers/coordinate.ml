open! Core

module T = struct
  type t = int * int [@@deriving compare, sexp_of, equal, hash]
end

include T
include Comparator.Make (T)

let neighbors (x, y) = [ (x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1) ]
let add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

type direction = Up | Down | Right | Left
[@@deriving sexp, compare, equal, hash]

let direction_to_offset = function
  | Up -> (0, -1)
  | Down -> (0, 1)
  | Left -> (-1, 0)
  | Right -> (1, 0)
