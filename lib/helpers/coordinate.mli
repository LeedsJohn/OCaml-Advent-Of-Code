open! Core

type t [@@deriving compare, equal, hash, sexp]

include Comparator.S with type t := t

val x : t -> int
val y : t -> int
val xy : t -> int * int
val make : x:int -> y:int -> t

(** (0, 0) *)
val origin : t

val distance : t -> t -> int
val add : t -> t -> t
val sub : t -> t -> t
val scale_down : t -> t
val scale : t -> int -> t
val neighbors : t -> t list
val rotate_right : t -> t
val rotate_left : t -> t
val turn_around : t -> t
val offsets : t list
val neighbors8 : t -> t list
val offsets8 : t list
val diagonals : t -> t list
val diagonal_offsets : t list
val shoot_ray : start:t -> dir:t -> length:int -> t list
