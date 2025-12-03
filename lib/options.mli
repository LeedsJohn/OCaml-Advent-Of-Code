open! Core
open! Async

module Run_type : sig
  type t =
    | Test
    | Real
    | Submit

  val arg : t Command.Arg_type.t
end

type t =
  { run_type : Run_type.t
  ; part : int
  }
