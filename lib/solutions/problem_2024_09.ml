open! Core

module Entry = struct
  type t = Empty of int | Full of int * int
end

module Entry_list = struct
  type t = Entry.t list

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
    aux [] 0 0

  let remove_empty_tail t = 
      List.drop_while t ~f:(function Entry.Empty _ -> true | Full _ -> false)


  let pop_tail t =
    let t = remove_empty_tail t
    in
    (List.last_exn t, List.drop_last_exn t)

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
          else aux (Empty (n - count) :: Full (count - n, id) :: acc) 0 tl
    in
    aux [] count t

  let int_list t =
    List.map t ~f:(function
      | Entry.Empty n -> List.init n ~f:(fun _ -> None)
      | Full (count, id) -> List.init count ~f:(fun _ -> Some id))
    |> List.join

  let checksum t =
    List.foldi (int_list t) ~init:0 ~f:(fun i acc n ->
        (Option.value n ~default:0 * i) + acc)

  let has_gap t = 
      List.exists (remove_empty_tail t) ~f:(function 
          | Empty _ -> true
          | Full _ -> false)
end

let part1 s =
     let t = Entry_list.of_string s in
     let rec aux t =
         if Entry_list.has_gap t then
             aux (Entry_list.shuffle_last_block t)
         else Entry_list.checksum t
     in
     aux t |> Int.to_string |> Ok
     

let part2 _ = Error (Error.of_string "Unimplemented")
