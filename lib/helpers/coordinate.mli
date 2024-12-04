open! Core

module T : sig
  module U : sig
    type t = int * int [@@deriving compare, sexp, equal, hash]
  end

  include module type of U
  include Comparator.S with type t := t
end

include module type of T

type coordinate = t

include Comparator.S with type t := t

val add : t -> t -> t
val scale : t -> int -> t
val neighbors : t -> Set.M(T).t
val offsets : Set.M(T).t
val neighbors8 : t -> Set.M(T).t
val offsets8 : Set.M(T).t
val diagonals : t -> Set.M(T).t
val diagonal_offsets : Set.M(T).t
val shoot_ray : start:t -> dir:t -> length:int -> t list

module Direction4 : sig
  type t = Up | Down | Right | Left [@@deriving sexp, compare, equal, hash]

  val to_offset : t -> coordinate
end
