open! Core

(* module Make (Elem : sig
     type t [@@deriving compare, sexp_of]
   end) =
   struct
     type t = | Empty | NonEmpty of (Elem.t * t) [@@deriving compare, sexp_of]

     let empty = Empty
     let is_empty t = compare empty t = 0

     let rec add (t : t) (e : Elem.t) = meld t (NonEmpty (e, empty))
     and meld (t1 : t) (t2 : t) =
         match t1, t2 with
         | Empty, t -> t
         | t, Empty -> t
         | ((NonEmpty ((x1, q1) as r1)), (NonEmpty ((x2, q2) as r2)))->
                 if Elem.compare x1 x2 < 0 then (NonEmpty (x1, add (NonEmpty r2) q1))
                 else NonEmpty (x2, add (NonEmpty r1 q2)

     let singleton e = [ e ]
     let of_list l = List.sort l ~compare:Elem.compare
     let add t e = List.merge t [ e ] ~compare:Elem.compare

     let peak_exn = function
       | e :: _ -> e
       | [] -> raise_s [%sexp ("Tried to peak on empty priority queue" : string)]

     let get_exn (t : t) : Elem.t * t =
       match t with
       | hd :: tl -> (hd, tl)
       | _ -> raise_s [%sexp ("Tried to pop on empty priority queue" : string)]
   end *)

module Make (Elem : sig
  type t [@@deriving compare, sexp_of]
end) =
struct
  type t = Elem.t list [@@deriving sexp_of]

  let empty = []
  let singleton e = [ e ]
  let of_list l = List.sort l ~compare:Elem.compare
  let add t e = List.merge t [ e ] ~compare:Elem.compare

  let peak_exn = function
    | e :: _ -> e
    | [] -> raise_s [%sexp ("Tried to peak on empty priority queue" : string)]

  let get_exn (t : t) : Elem.t * t =
    match t with
    | hd :: tl -> (hd, tl)
    | _ -> raise_s [%sexp ("Tried to pop on empty priority queue" : string)]
end
