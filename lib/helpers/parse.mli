open! Core

val take_int : ?default:int -> string -> int * string

(* ignores characters until finds a number *)
val take_next_int : ?default:int -> string -> int * string
val line_numbers : string -> int list
