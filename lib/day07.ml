open! Core

let day = 7
let year = 2023

type t = { cards : string; hand_score : int; bid : int } [@@deriving sexp]

let get_char_counts s =
  String.fold s
    ~init:(Map.empty (module Char))
    ~f:(fun acc c ->
      match Map.find acc c with
      | None -> Map.add_exn acc ~key:c ~data:1
      | Some n -> Map.update acc c ~f:(fun _ -> n + 1))
  |> Map.data
  |> List.sort ~compare:(fun n1 n2 -> -(n1 - n2))

let five_kind char_counts = List.hd_exn char_counts |> Int.equal 5
let four_kind char_counts = List.hd_exn char_counts |> Int.equal 4

let full_house char_counts =
  List.hd_exn char_counts = 3
  && List.tl_exn char_counts |> List.hd_exn |> Int.equal 2

let three_kind char_counts = List.hd_exn char_counts |> Int.equal 3

let two_pair char_counts =
  List.hd_exn char_counts = 2
  && List.tl_exn char_counts |> List.hd_exn |> Int.equal 2

let one_pair char_counts = List.hd_exn char_counts |> Int.equal 2
let high_card _ = true

let get_hand_score s get_char_counts =
  let i, _ =
    List.findi_exn
      [
        five_kind (get_char_counts s);
        four_kind (get_char_counts s);
        full_house (get_char_counts s);
        three_kind (get_char_counts s);
        two_pair (get_char_counts s);
        one_pair (get_char_counts s);
        high_card (get_char_counts s);
      ]
      ~f:(fun _ b -> b)
  in
  6 - i

let compare_hands t1 t2 card_order =
  if String.equal t1.cards t2.cards then 0
  else if t1.hand_score <> t2.hand_score then t1.hand_score - t2.hand_score
  else
    let dif_i =
      List.find_exn (List.range 0 5) ~f:(fun i ->
          not (Char.equal (String.get t1.cards i) (String.get t2.cards i)))
    in
    let card_score cards i =
      String.findi card_order ~f:(fun _ c -> Char.equal (String.get cards i) c)
      |> Option.value_exn |> fst
    in
    card_score t1.cards dif_i - card_score t2.cards dif_i

let parse_input lines get_char_counts =
  List.map lines ~f:(fun hand ->
      let hand = String.split hand ~on:' ' in
      let cards = List.hd_exn hand in
      let bid = List.tl_exn hand |> List.hd_exn |> Int.of_string in
      { cards; hand_score = get_hand_score cards get_char_counts; bid })

let part1 fname =
  parse_input (In_channel.read_lines fname) get_char_counts
  |> List.sort ~compare:(fun t1 t2 -> compare_hands t1 t2 "23456789TJQKA")
  |> List.foldi ~init:0 ~f:(fun i acc hand -> acc + ((i + 1) * hand.bid))
  |> Int.to_string

let get_char_counts2 s =
  let char_counts =
    String.fold s
      ~init:(Map.empty (module Char))
      ~f:(fun acc c ->
        if Char.equal c 'J' then acc
        else
          match Map.find acc c with
          | None -> Map.add_exn acc ~key:c ~data:1
          | Some n -> Map.update acc c ~f:(fun _ -> n + 1))
    |> Map.data
    |> List.sort ~compare:(fun n1 n2 -> -(n1 - n2))
  in
  let joker_count = String.count s ~f:(Char.equal 'J') in
  if List.length char_counts = 0 then [ 5 ]
  else (List.hd_exn char_counts + joker_count) :: List.tl_exn char_counts

let part2 fname =
  parse_input (In_channel.read_lines fname) get_char_counts2
  |> List.sort ~compare:(fun t1 t2 -> compare_hands t1 t2 "J23456789TQKA")
  |> List.foldi ~init:0 ~f:(fun i acc hand -> acc + ((i + 1) * hand.bid))
  |> Int.to_string

let%expect_test "stuff" =
  let hands =
    parse_input
      ({|32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483|}
     |> String.split_lines)
      get_char_counts
    |> List.sort ~compare:(fun t1 t2 -> compare_hands t1 t2 "23456789TJQKA")
  in
  print_s [%sexp (hands : t list)];
  [%expect
    {|
    (((cards 32T3K) (hand_score 1) (bid 765))
     ((cards KTJJT) (hand_score 2) (bid 220))
     ((cards KK677) (hand_score 2) (bid 28))
     ((cards T55J5) (hand_score 3) (bid 684))
     ((cards QQQJA) (hand_score 3) (bid 483))) |}];
  ()
