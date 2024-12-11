open! Core

(* let rec gcd a b =
     if a = b then a else if a > b then gcd (a - b) b else gcd a (b - a)

   let primes_up_to n =
     let nums = Array.init (n + 1) ~f:(fun _ -> true) in
     nums.(0) <- false;
     nums.(1) <- false;
     for i = 2 to n do
       if not nums.(i) then ()
       else
         let j = ref (i + i) in
         while !j <= n do
           nums.(!j) <- false;
           j := !j + i
         done
     done;
     List.of_array nums

   let is_prime n =
     List.for_all (List.range 2 ((n / 2) + 1)) ~f:(fun i -> n % i <> 0) *)

(* let prime_factorization n = *)
