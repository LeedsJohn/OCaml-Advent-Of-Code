open! Core

module type S = sig
  type t [@@deriving compare, sexp_of, hash]

  val get_neighbor_states : t -> t list
end

module type Extension = sig
  type t

  val bfs : ?verbose:bool -> t -> is_goal:(t -> bool) -> int
  val djikstra : t -> is_goal:(t -> bool) -> t
end

module type EXTENDER = functor (Arg : S) -> Extension with type t := Arg.t

module type Intf = sig
  module type S = S
  module type EXTENDER = EXTENDER

  module Extend : EXTENDER
end
