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
  val peek_exn : t -> elem
  val peek : t -> elem option
  val get_exn : t -> elem * t
  val get : t -> (elem * t) option
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
