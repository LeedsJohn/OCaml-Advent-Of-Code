open! Core

type t = int * int [@@deriving compare, sexp_of, equal, hash]

include Comparator.S with type t := t

val add : t -> t -> t
val neighbors : t -> t list

type direction = Up | Down | Right | Left
[@@deriving sexp, compare, equal, hash]

val direction_to_offset : direction -> t
