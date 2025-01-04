open! Core

let rec gcd a b =
  if a = b then a else if a > b then gcd (a - b) b else gcd a (b - a)

let lcm a b = a / gcd a b * b
let lcm_list nums = List.fold nums ~init:1 ~f:lcm

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

let int_to_bin_string n =
  if n < 0 then
    raise_s
      [%message "can't convert negative numbers to binary string" (n : int)];
  let rec aux acc n =
    if n = 0 then List.rev acc |> String.of_list
    else if n % 2 = 1 then aux ('1' :: acc) (n / 2)
    else aux ('0' :: acc) (n / 2)
  in
  aux [] n

let bin_string_to_int s =
  String.foldi s ~init:0 ~f:(fun i acc c ->
      if Char.equal c '0' then acc
      else if Char.equal c '1' then acc + Int.pow 2 i
      else raise_s [%message "invalid character in binary string" (c : char)])

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

let%expect_test "bin strings" =
  let nums =
    List.map (List.range 0 13) ~f:(fun n ->
        bin_string_to_int (int_to_bin_string n))
  in
  print_s [%sexp (nums : int list)];
  [%expect {| (0 1 2 3 4 5 6 7 8 9 10 11 12) |}];
  ()
