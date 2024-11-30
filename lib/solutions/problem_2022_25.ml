open! Core

(* let dec_add n1 n2 =
   let rec fix n1 n2 =
     if List.length n1 = List.length n2 then (n1, n2)
     else if List.length n1 > List.length n2 then fix n2 n1
     else fix (0 :: n1) n2
   in
   let n1, n2 = fix n1 n2 in
   let res, carry =
     List.fold
       (List.zip_exn (List.rev n1) (List.rev n2))
       ~init:([], 0)
       ~f:(fun (acc, carry) (n1, n2) ->
         let num = n1 + n2 + carry in
         let num, carry = if num >= 10 then (num - 10, 1) else (num, 0) in
         (num :: acc, carry))
   in
   match carry with 1 -> 1 :: res | _ -> res *)

let dec_to_5 (n : Z.t) =
  let rec aux n acc =
    if Z.equal n Z.zero then acc
    else
      let remainder = Z.( mod ) n (Z.of_int 5) in
      aux Z.((n - remainder) / of_int 5) (remainder :: acc)
  in
  aux n [] |> List.map ~f:Z.to_int

let _five_to_dec n =
  List.foldi n ~init:0 ~f:(fun i acc num -> (Int.pow 5 i * num) + acc)

let five_to_snafu n =
  let res, carry =
    List.fold (List.rev n) ~init:([], 0) ~f:(fun (acc, carry) num ->
        let num = num + carry in
        let num, next_carry = if num > 2 then (num - 5, 1) else (num, 0) in
        let digit =
          match num with
          | -2 -> '='
          | -1 -> '-'
          | 0 -> '0'
          | 1 -> '1'
          | 2 -> '2'
          | _ ->
              raise_s
                [%message
                  "bad digit" ~digit:(num : int) ~carry:(next_carry : int)]
        in
        (digit :: acc, next_carry))
  in
  match carry with 1 -> '1' :: res | 2 -> '2' :: res | _ -> res

let snafu_to_ten n =
  List.foldi (List.rev n) ~init:Z.zero ~f:(fun i acc num ->
      let v =
        match num with
        | '=' -> -2
        | '-' -> -1
        | c -> Char.to_string c |> Int.of_string
      in
      Z.(acc + (pow (of_int 5) i * of_int v)))

let dec_to_snafu n = dec_to_5 n |> five_to_snafu

let part1 s =
  let lines = String.split_lines s |> List.map ~f:String.to_list in
  let total =
    List.fold lines ~init:Z.zero ~f:(fun acc snafu ->
        Z.(acc + snafu_to_ten snafu))
  in
  print_endline (Z.to_string total);
  (* let five = List.to_string ~f:(fun n -> Int.to_string n) (dec_to_5 total) in *)
  let res = dec_to_snafu total in
  Ok (String.of_list res)

let part2 _ = Error (Error.of_string "Unimplemented")
