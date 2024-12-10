open! Core

(* this is way slower than it should be.
   I know I made some sacrifices to keep it functional but it's making a much bigger
   difference than I expected. My gut feeling is this should take on the order of
   tenths of a second but it's taking like 7 seconds.
   It might be fun to come back and rewrite this more nicely *)

module Entry = struct
  type t = Empty of int | Full of int * int [@@deriving sexp]
end

module Entry_list = struct
  type t = Entry.t list [@@deriving sexp]

  let fix_stuff t =
    let rec aux acc = function
      | [] -> List.rev acc
      | (Entry.Empty n1 as hd) :: tl -> (
          match acc with
          | Entry.Empty n2 :: tl2 -> aux (Empty (n1 + n2) :: tl2) tl
          | _ -> aux (hd :: acc) tl)
      | (Full (n1, id1) as hd) :: tl -> (
          match acc with
          | Entry.Full (n2, id2) :: tl2 ->
              if id1 = id2 then aux (Full (n1 + n2, id1) :: tl2) tl
              else aux (hd :: acc) tl
          | _ -> aux (hd :: acc) tl)
    in
    let t = aux [ List.hd_exn t ] (List.tl_exn t) in
    List.filter t ~f:(function Empty 0 | Full (0, _) -> false | _ -> true)
    |> List.rev
    |> List.drop_while ~f:(function Entry.Empty _ -> true | _ -> false)
    |> List.rev

  let of_string s : t =
    let rec aux acc i id =
      if i >= String.length s then List.rev acc
      else if i % 2 = 0 then
        aux
          (Entry.Full (Char.get_digit_exn (String.get s i), id) :: acc)
          (i + 1) (id + 1)
      else
        aux
          (Entry.Empty (Char.get_digit_exn (String.get s i)) :: acc)
          (i + 1) id
    in
    aux [] 0 0 |> fix_stuff

  let int_list t =
    List.map t ~f:(function
      | Entry.Empty n -> List.init n ~f:(fun _ -> None)
      | Full (count, id) -> List.init count ~f:(fun _ -> Some id))
    |> List.join

  let pop_tail t : Entry.t * t = (List.last_exn t, List.drop_last_exn t)

  let shuffle_last_block t =
    let last_block, t = pop_tail t in
    let id, count =
      match last_block with
      | Full (count, id) -> (id, count)
      | Empty _ ->
          raise_s [%message "shouldn't be possible to get an empty thing here"]
    in
    let rec aux acc count = function
      | [] ->
          if count = 0 then List.rev acc
          else List.rev (Entry.Full (count, id) :: acc)
      | (Entry.Full _ as hd) :: tl -> aux (hd :: acc) count tl
      | Empty n :: tl ->
          if count = n then aux (Full (count, id) :: acc) 0 tl
          else if count > n then aux (Full (n, id) :: acc) (count - n) tl
          else aux (Empty (n - count) :: Full (count, id) :: acc) 0 tl
    in
    aux [] count t |> fix_stuff

  let shuffle_last_block' t =
    let count, id, t =
      List.fold (List.rev t) ~init:(-1, -1, []) ~f:(fun (count, id, acc) t ->
          match t with
          | Entry.Full (c, i) as e ->
              if id = -1 && i >= 0 then (c, i, Entry.Empty c :: acc)
              else (count, id, e :: acc)
          | e -> (count, id, e :: acc))
    in
    let rec aux acc added = function
      | [] -> List.rev acc
      | (Entry.Full _ as hd) :: tl -> aux (hd :: acc) added tl
      | (Empty n as hd) :: tl ->
          if (not added) && n >= count then
            aux (Empty (n - count) :: Full (count, -id) :: acc) true tl
          else aux (hd :: acc) added tl
    in
    let t = aux [] false t in
    fix_stuff t

  let checksum t =
    List.foldi (int_list t) ~init:0 ~f:(fun i acc n ->
        (Option.value n ~default:0 * i) + acc)

  let has_gap t =
    List.exists (fix_stuff t) ~f:(function Empty _ -> true | Full _ -> false)

  let part2done t =
    List.for_all t ~f:(function Entry.Full (_, id) -> id <= 0 | _ -> true)
end

let part1 s =
  let t = Entry_list.of_string s in
  let rec aux t =
    if Entry_list.has_gap t then aux (Entry_list.shuffle_last_block t)
    else Entry_list.checksum t
  in
  aux t |> Int.to_string |> Ok

let part2 s =
  let t = Entry_list.of_string s in
  let rec aux t =
    if Entry_list.part2done t then
      Entry_list.checksum
        (List.map t ~f:(function
          | Entry.Empty _ as e -> e
          | Full (n, id) -> Full (n, -id)))
    else aux (Entry_list.shuffle_last_block' t)
  in
  aux t |> Int.to_string |> Ok
