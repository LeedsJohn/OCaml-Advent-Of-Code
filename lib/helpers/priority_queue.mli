include Priority_queue_intf.Intf
(* open! Core

   module type Pq_intf = sig
     type t
     type elem

     val compare : t -> t -> int
     val sexp_of : t -> Sexp.t
     val empty : t
     val is_empty : t -> bool
     val add : t -> elem -> t
     val singleton : elem -> t
   end

   module Make (Elem : sig
     type t [@@deriving compare, sexp_of]
   end) : sig
     type t [@@deriving compare, sexp_of]

     val empty : t
     val is_empty : t -> bool
     val singleton : Elem.t -> t
     val of_list : Elem.t list -> t
     val add : t -> Elem.t -> t
     val peak_exn : t -> Elem.t
     val get_exn : t -> Elem.t * t
   end *)
