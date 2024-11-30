open! Core
open! Async

val run : day:int -> year:int -> options:Options.t -> unit Deferred.t
