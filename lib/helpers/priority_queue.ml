open! Core

(* Source: Optimal Purely Functional Priority Queues by Gerth Stolting Brodal and
   Chris Okasaki *)

include Priority_queue_intf

module Make : MAKER =
functor
  (Elem : sig
     type t [@@deriving compare, sexp_of]
   end)
  ->
  struct
    type tree = Node of Elem.t * int * tree list [@@deriving compare, sexp_of]
    type t = Pq of tree list [@@deriving compare, sexp_of]

    let sexp_of = sexp_of_t
    let root (Node (x, _, _) : tree) : Elem.t = x
    let rank (Node (_, r, _) : tree) : int = r

    let link (Node (x1, r1, c1) as t1 : tree) (Node (x2, r2, c2) as t2 : tree) :
        tree =
      if Elem.compare x1 x2 <= 0 then Node (x1, r1 + 1, t2 :: c1)
      else Node (x2, r2 + 1, t1 :: c2)

    let rec ins (ts : t) (t : tree) =
      match ts with
      | Pq [] -> [ t ]
      | Pq (t' :: ts) ->
          if rank t < rank t' then t :: t' :: ts else ins (Pq ts) (link t t')

    let empty = Pq []
    let is_empty = function Pq [] -> true | _ -> false
    let add (t : t) (x : Elem.t) : t = Pq (ins t (Node (x, 0, [])))
    let singleton x = add empty x
    let of_list xs = List.fold xs ~init:empty ~f:(fun acc x -> add acc x)

    let rec meld (Pq t1) (Pq t2) : t =
      match (t1, t2) with
      | [], ts | ts, [] -> Pq ts
      | t1 :: ts1, t2 :: ts2 ->
          let aux t1 t2 ts1 ts2 =
            let (Pq ts) = meld (Pq ts1) (Pq (t2 :: ts2)) in
            Pq (t1 :: ts)
          in
          if rank t1 < rank t2 then aux t1 t2 ts1 ts2
          else if rank t2 < rank t1 then aux t2 t1 ts2 ts1
          else Pq (ins (meld (Pq ts1) (Pq ts2)) (link t1 t2))

    let rec peak_exn = function
      | Pq [] -> raise_s [%message "Attempted to peak empty priority queue"]
      | Pq [ t ] -> root t
      | Pq (t :: ts) ->
          let x = peak_exn (Pq ts) in
          if Elem.compare (root t) x <= 0 then root t else x

    let get_exn (t : t) =
      let res = peak_exn t in
      let new_pq =
        match t with
        | Pq [] ->
            raise_s [%message "Attempted to get from empty priority queue"]
        | Pq ts ->
            let rec get_min = function
              | Pq [] -> raise_s [%message "this should be impossible"]
              | Pq [ t ] -> (t, [])
              | Pq (t :: ts) ->
                  let t', ts' = get_min (Pq ts) in
                  if Elem.compare (root t) (root t') <= 0 then (t, ts)
                  else (t', t :: ts')
            in
            let Node (_, _, c), ts = get_min (Pq ts) in
            meld (Pq (List.rev c)) (Pq ts)
      in
      (res, new_pq)
  end
