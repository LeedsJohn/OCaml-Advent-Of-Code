open! Core

module Make (Elem : sig
  type t [@@deriving compare, sexp_of]
end) =
struct
  type t = Elem.t list

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
