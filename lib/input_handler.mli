open! Core
open! Async

val get_input : day:int -> year:int -> test:bool -> string Deferred.t
