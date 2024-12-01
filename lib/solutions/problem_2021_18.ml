open! Core
open! Helpers

(* module Snailfish = struct
     type t = Num of int | Pair of t * t [@@deriving sexp]

     let rec get_pair s =
       let left, s = of_string' (String.drop_prefix s 1) in
       let right, s = of_string' (String.drop_prefix s 1) in
       (Pair (left, right), String.drop_prefix s 1)

     and get_num s =
       let n, s = Parse.take_int_exn s in
       (Num n, s)

     and of_string' s =
       match String.get s 0 with '[' -> get_pair s | _ -> get_num s

     let of_string s = fst (of_string' s)
     let _add t1 t2 = Pair (t1, t2)

     let _explode_or_split t =
       let rec aux t depth =
         match (depth >= 4, t) with
         | true, Pair (Num _, Num _) -> Some `Explode
         | _, Num n -> if n >= 10 then Some `Split else None
         | _, Pair (t1, t2) -> (
             match aux t1 (depth + 1) with
             | Some _ as res -> res
             | None -> aux t2 (depth + 1))
       in
       aux t 0

     let split t =
       let rec aux t found =
         if found then (t, true)
         else
           match t with
           | Num n ->
               if n >= 10 then (Pair (Num (n / 2), Num ((n / 2) + (n % 2))), true)
               else (Num n, false)
           | Pair (l, r) ->
               let l, found = aux l false in
               if found then (Pair (l, r), true)
               else
                 let r, found = aux r false in
                 (Pair (l, r), found)
       in
       aux t false |> fst

       let explode t =
           let rec aux t depth =
               match depth, t with
               | 3, Pair (Num l, Num r) -> Num 0, Some l, Some r
               | _, Num n -> Num n, None, None
               | _, Pair (tl, tr) ->
                       let res, l, r = aux t (depth + 1) in
                       match




   end


   let%expect_test "parse" =
     print_s [%sexp (Snailfish.of_string "[1,2]" : Snailfish.t)];
     [%expect {| (Pair (Num 1) (Num 2)) |}];
     print_s
       [%sexp
         (Snailfish.of_string
            "[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]"
           : Snailfish.t)];
     [%expect
       {|
       (Pair
        (Pair (Pair (Pair (Num 1) (Num 3)) (Pair (Num 5) (Num 3)))
         (Pair (Pair (Num 1) (Num 3)) (Pair (Num 8) (Num 7))))
        (Pair (Pair (Pair (Num 4) (Num 9)) (Pair (Num 6) (Num 9)))
         (Pair (Pair (Num 8) (Num 2)) (Pair (Num 7) (Num 3)))))
       |}]

   let%expect_test "split" =
     let t = Snailfish.of_string "[[[[0,7],4],[15,[0,13]]],[1,1]]" in

     print_s [%sexp (Snailfish.split t : Snailfish.t)];
     [%expect
       {|
       (Pair
        (Pair (Pair (Pair (Num 0) (Num 7)) (Num 4))
         (Pair (Pair (Num 7) (Num 8)) (Pair (Num 0) (Num 13))))
        (Pair (Num 1) (Num 1)))
       |}] *)

let part1 _ = Error (Error.of_string "Unimplemented")
let part2 _ = Error (Error.of_string "Unimplemented")
