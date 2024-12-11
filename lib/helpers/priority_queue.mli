open! Core

module Make (Elem : sig
  type t [@@deriving compare, sexp_of]
end) : sig
  type t [@@deriving sexp_of]

  val empty : t
  val singleton : Elem.t -> t
  val of_list : Elem.t list -> t
  val add : t -> Elem.t -> t
  val peak_exn : t -> Elem.t
  val get_exn : t -> Elem.t * t
end
