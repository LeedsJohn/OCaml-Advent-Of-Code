open! Core

let rec gcd a b =
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
  Array.foldi nums ~init:[] ~f:(fun i acc is_prime ->
      if is_prime then i :: acc else acc)
  |> List.rev

let is_prime n =
  n >= 2 && List.for_all (List.range 2 ((n / 2) + 1)) ~f:(fun i -> n % i <> 0)

let prime_factorization n =
  let rec aux acc n =
    match List.find (List.range 2 (n + 1)) ~f:(fun p -> n % p = 0) with
    | None -> acc
    | Some div ->
        aux
          (Map.update acc div ~f:(function None -> 1 | Some num -> num + 1))
          (n / div)
  in
  aux (Map.empty (module Int)) n

let%expect_test "primes up to" =
  print_s [%sexp (primes_up_to 97 : int list)];
  [%expect
    {| (2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97) |}];
  ()

let%expect_test "is_prime" =
  let primes = List.filter (List.range 0 100) ~f:is_prime in
  print_s [%sexp (primes : int list)];
  [%expect
    {| (2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97) |}];
  ()

let%expect_test "prime factorization" =
  print_s
    [%sexp (prime_factorization (2 * 3 * 5 * 49 * 17 * 97) : int Map.M(Int).t)];
  [%expect {| ((2 1) (3 1) (5 1) (7 2) (17 1) (97 1)) |}];
  ()
