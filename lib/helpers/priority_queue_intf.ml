open! Core

module type S = sig
  type t
  type elem

  val sexp_of : t -> Sexp.t
  val empty : t
  val is_empty : t -> bool
  val singleton : elem -> t
  val of_list : elem list -> t
  val add : t -> elem -> t
  val peak_exn : t -> elem
  val get_exn : t -> elem * t
end

module type MAKER = functor
  (Elem : sig
     type t [@@deriving compare, sexp_of]
   end)
  -> S with type elem := Elem.t

module type Intf = sig
  module type S = S
  module type MAKER = MAKER

  module Make : MAKER
end
