open! Core

let get_next s i =
  let i = ref i in
  while
    not
      (Md5.digest_string (s ^ Int.to_string !i)
       |> Md5.to_hex
       |> String.is_prefix ~prefix:"00000")
  do
    i := !i + 1
  done;
  Md5.digest_string (s ^ Int.to_string !i) |> Md5.to_hex, !i
;;

let part1 s =
  let rec aux acc i =
    if List.length acc = 8
    then List.rev acc |> String.of_list
    else (
      let hashed, i = get_next s i in
      aux (String.get hashed 5 :: acc) (i + 1))
  in
  aux [] 0
;;

let part2 s =
  let res = Array.create ~len:8 None in
  let rec aux i =
    if Array.for_all res ~f:Option.is_some
    then ()
    else (
      let hashed, i = get_next s i in
      match String.get hashed 5 |> Char.get_digit with
      | None -> aux (i + 1)
      | Some j ->
        if j < 8 && Option.is_none res.(j) then res.(j) <- Some (String.get hashed 6);
        aux (i + 1))
  in
  aux 0;
  Array.map res ~f:(fun c -> Option.value_exn c) |> String.of_array
;;

let%expect_test "" =
  print_s [%sexp (Md5.digest_string "abc3231929" |> Md5.to_hex : string)];
  [%expect {| 00000155f8105dff7f56ee10fa9b9abd |}];
  print_s [%sexp (Md5.digest_string "abc5017308" |> Md5.to_hex : string)];
  [%expect {| 000008f82c5b3924a1ecbebf60344e00 |}];
  ()
;;
