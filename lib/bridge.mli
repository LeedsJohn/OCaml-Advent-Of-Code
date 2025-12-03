open! Core
open! Async

module Submission_result : sig
  type t [@@deriving sexp]
end

val fetch_real_input : day:int -> year:int -> string Deferred.t

val submit_solution
  :  day:int
  -> year:int
  -> part:int
  -> answer:string
  -> Submission_result.t Deferred.t
