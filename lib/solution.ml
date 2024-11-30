open! Core

module type T = sig
  val part1 : string -> string Or_error.t
  val part2 : string -> string Or_error.t
end
