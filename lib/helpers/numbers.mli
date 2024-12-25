open! Core

val gcd : int -> int -> int

(* returns primes in sorted order *)
val primes_up_to : int -> int list
val is_prime : int -> bool

(* map of prime factor : power *)
val prime_factorization : int -> int Map.M(Int).t
val int_to_bin_string : int -> string
val bin_string_to_int : string -> int
