open! Core

let to_binary' c =
  let letters = String.to_list "0123456789ABCDEF" in
  let res =
    [ "0000"
    ; "0001"
    ; "0010"
    ; "0011"
    ; "0100"
    ; "0101"
    ; "0110"
    ; "0111"
    ; "1000"
    ; "1001"
    ; "1010"
    ; "1011"
    ; "1100"
    ; "1101"
    ; "1110"
    ; "1111"
    ]
  in
  List.Assoc.find_exn (List.zip_exn letters res) ~equal:Char.equal c
;;

let to_int bin start stop =
  List.foldi
    (List.rev (List.range start stop))
    ~init:0
    ~f:(fun j acc i ->
      let m = String.get bin i |> Char.get_digit_exn in
      acc + (m * Int.pow 2 j))
;;

module Packet = struct
  type payload =
    | Literal of int
    | Operator of t list
  [@@deriving sexp]

  and t =
    { version : int
    ; id_ : int
    ; payload : payload
    ; start : int
    ; end_ : int
    }
  [@@deriving sexp]

  let rec get_value t =
    let get_value' l fn init =
      List.fold l ~init ~f:(fun acc packet -> fn acc (get_value packet))
    in
    let get_value'' l fn =
      match l with
      | [ a; b ] -> fn (get_value a) (get_value b) |> Bool.to_int
      | l -> raise_s [%message "list without two values" (l : t list)]
    in
    match t.id_, t.payload with
    | 4, Literal n -> n
    | 0, Operator l -> get_value' l Int.( + ) 0
    | 1, Operator l -> get_value' l Int.( * ) 1
    | 2, Operator l -> get_value' l Int.min Int.max_value
    | 3, Operator l -> get_value' l Int.max Int.min_value
    | 5, Operator l -> get_value'' l Int.( > )
    | 6, Operator l -> get_value'' l Int.( < )
    | 7, Operator l -> get_value'' l Int.equal
    | id_, payload ->
      raise_s
        [%message "bad id / payload combo" ~id_:(id_ : int) ~payload:(payload : payload)]
  ;;

  let get_metadata s start =
    let version = to_int s start (start + 3) in
    let id_ = to_int s (start + 3) (start + 6) in
    version, id_
  ;;

  let get_length s start =
    match String.get s (start + 6) with
    | '0' -> `Bits (to_int s (start + 7) (start + 22)), start + 22
    | _ -> `Packets (to_int s (start + 7) (start + 18)), start + 18
  ;;

  let get_literal s start =
    let rec aux acc i =
      let next_section = String.slice s (i + 1) (i + 5) in
      match String.get s i with
      | '1' ->
        let extra, p = aux acc (i + 5) in
        acc ^ next_section ^ extra, p
      | _ -> acc ^ next_section, i + 5
    in
    let p, end_ = aux "" (start + 6) in
    to_int p 0 (String.length p), end_
  ;;

  let rec get_operator s start =
    match get_length s start with
    | `Bits n, start ->
      let payload = get_n_bits s ~start ~n in
      payload, start + n
    | `Packets n, start ->
      let payload = get_n_packets s ~start ~n in
      let thing = List.rev payload |> List.hd_exn in
      payload, thing.end_

  and get_n_bits s ~start ~n =
    let end_point = start + n in
    let rec aux acc start =
      let next_packet = of_bin s start in
      if next_packet.end_ >= end_point
      then List.rev (next_packet :: acc)
      else aux (next_packet :: acc) next_packet.end_
    in
    aux [] start

  and get_n_packets s ~start ~n =
    let rec aux acc start count =
      let next_packet = of_bin s start in
      if count = n
      then List.rev (next_packet :: acc)
      else aux (next_packet :: acc) next_packet.end_ (count + 1)
    in
    aux [] start 1

  and of_bin bin start =
    let version, id_ = get_metadata bin start in
    print_s [%message "Parsing" ~start:(start : int) ((version, id_) : int * int)];
    match id_ with
    | 4 ->
      print_endline "Literal";
      let payload, end_ = get_literal bin start in
      { version; id_; payload = Literal payload; start; end_ }
    | _ ->
      print_endline "Operator";
      let payload, end_ = get_operator bin start in
      { version; id_; payload = Operator payload; start; end_ }
  ;;
end

let to_binary s = String.fold s ~init:"" ~f:(fun acc c -> acc ^ to_binary' c)

let part1 s =
  let packet = Packet.of_bin (to_binary s) 0 in
  let rec aux acc (packet : Packet.t) =
    match packet.payload with
    | Literal _ -> acc + packet.version
    | Operator sub_packets ->
      List.fold sub_packets ~init:(acc + packet.version) ~f:(fun acc packet ->
        aux acc packet)
  in
  Int.to_string (aux 0 packet)
;;

let part2 s =
  let packet = Packet.of_bin (to_binary s) 0 in
  Packet.get_value packet |> Int.to_string
;;

let%expect_test "to_int" =
  let s = "110100" in
  print_s [%sexp (to_int s 0 3 : int)];
  print_s [%sexp (to_int s 3 6 : int)];
  [%expect
    {|
      6
      4
      |}]
;;

let%expect_test "literal" =
  let bin = to_binary "D2FE28" in
  print_endline bin;
  [%expect {| 110100101111111000101000 |}];
  print_s [%sexp (Packet.of_bin bin 0 : Packet.t)];
  [%expect
    {|
    (Parsing (start 0) ("(version, id_)" (6 4)))
    Literal
    ((version 6) (id_ 4) (payload (Literal 2021)) (start 0) (end_ 21))
    |}]
;;

let%expect_test "operator" =
  let bin = to_binary "38006F45291200" in
  print_s [%sexp (bin : string)];
  [%expect {| 00111000000000000110111101000101001010010001001000000000 |}];
  print_s [%sexp (Packet.get_length bin 0 : [ `Bits of int | `Packets of int ] * int)];
  [%expect {| ((Bits 27) 22) |}];
  print_s [%sexp (Packet.of_bin bin 0 : Packet.t)];
  [%expect
    {|
    (Parsing (start 0) ("(version, id_)" (1 6)))
    Operator
    (Parsing (start 22) ("(version, id_)" (6 4)))
    Literal
    (Parsing (start 33) ("(version, id_)" (2 4)))
    Literal
    ((version 1) (id_ 6)
     (payload
      (Operator
       (((version 6) (id_ 4) (payload (Literal 10)) (start 22) (end_ 33))
        ((version 2) (id_ 4) (payload (Literal 20)) (start 33) (end_ 49)))))
     (start 0) (end_ 49))
    |}]
;;

let%expect_test "operator2" =
  let bin = to_binary "EE00D40C823060" in
  print_s [%sexp (bin : string)];
  [%expect {| 11101110000000001101010000001100100000100011000001100000 |}];
  print_s [%sexp (Packet.get_length bin 0 : [ `Bits of int | `Packets of int ] * int)];
  [%expect {| ((Packets 3) 18) |}];
  print_s [%sexp (Packet.of_bin bin 0 : Packet.t)];
  [%expect
    {|
    (Parsing (start 0) ("(version, id_)" (7 3)))
    Operator
    (Parsing (start 18) ("(version, id_)" (2 4)))
    Literal
    (Parsing (start 29) ("(version, id_)" (4 4)))
    Literal
    (Parsing (start 40) ("(version, id_)" (1 4)))
    Literal
    ((version 7) (id_ 3)
     (payload
      (Operator
       (((version 2) (id_ 4) (payload (Literal 1)) (start 18) (end_ 29))
        ((version 4) (id_ 4) (payload (Literal 2)) (start 29) (end_ 40))
        ((version 1) (id_ 4) (payload (Literal 3)) (start 40) (end_ 51)))))
     (start 0) (end_ 51))
    |}]
;;
