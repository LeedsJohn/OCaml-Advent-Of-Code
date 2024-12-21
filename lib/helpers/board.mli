open! Core

type 'a t = 'a Map.M(Coordinate).t [@@deriving equal, compare, sexp_of]

val of_string : string -> char t
val min_coordinates : 'a t -> int * int
val max_coordinates : 'a t -> int * int
val to_string : char t -> string
