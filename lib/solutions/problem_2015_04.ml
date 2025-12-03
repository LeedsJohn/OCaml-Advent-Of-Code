open! Core

let find_prefix s ~prefix =
  let rec aux i =
    let s = Md5.digest_string (s ^ Int.to_string i) |> Md5.to_hex in
    if String.is_prefix s ~prefix then i else aux (i + 1)
  in
  aux 1
;;

let part1 s = find_prefix s ~prefix:"00000" |> Int.to_string
let part2 s = find_prefix s ~prefix:"000000" |> Int.to_string

let%expect_test "md5" =
  print_s [%sexp (Md5.digest_string "abcdef609043" |> Md5.to_hex : string)];
  [%expect {| 000001dbbfa3a5c83a2d506429c7b00e |}];
  ()
;;
