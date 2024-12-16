open! Core
open! Async

val run :
  day:int -> year:int -> tag:string -> options:Options.t -> unit Deferred.t
