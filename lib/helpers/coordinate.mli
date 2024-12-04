open! Core

type t = int * int [@@deriving compare, sexp_of, equal, hash]

include Comparator.S with type t := t

val add : t -> t -> t
val multiply : t -> int -> t
val neighbors : t -> t list
val offsets : t list
val neighbors8: t -> t list
val offsets8: t list
val diagonals : t -> t list
val diagonal_offsets : t list
val shoot_ray : start:t -> dir:t -> length:int -> t list

type direction = Up | Down | Right | Left
[@@deriving sexp, compare, equal, hash]

val direction_to_offset : direction -> t
