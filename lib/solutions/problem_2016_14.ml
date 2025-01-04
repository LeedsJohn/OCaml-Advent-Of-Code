open! Core

let first_run_of_n s n =
  String.to_list "0123456789abcdef"
  |> List.filter_map ~f:(fun c ->
         let goal_s = String.make n c in
         let i = String.substr_index s ~pattern:goal_s in
         match i with None -> None | Some i -> Some (c, i))
  |> List.min_elt ~compare:(fun (_, n1) (_, n2) -> Int.compare n1 n2)
  |> Option.map ~f:fst

let has_run_of_n s c n = String.is_substring s ~substring:(String.make n c)

let find_n_keys salt n hashfn =
  let get_new_keys open_stuff s =
    Map.filteri open_stuff ~f:(fun ~key:c ~data:_ -> has_run_of_n s c 5)
    |> Map.to_alist
  in
  let filter_open_stuff open_stuff used_keys =
    Map.filteri open_stuff ~f:(fun ~key:c ~data:_ ->
        not (List.mem used_keys c ~equal:Char.equal))
  in
  let rec aux acc open_stuff i last_thing =
    if i - 1000 >= last_thing then acc
    else
      let last_thing =
        if List.length acc >= n && last_thing = Int.max_value then
          List.max_elt acc ~compare:Int.compare |> Option.value_exn
        else last_thing
      in
      let open_stuff =
        Map.map open_stuff ~f:(fun positions ->
            List.filter positions ~f:(fun j -> j + 1000 >= i))
        |> Map.filter ~f:(fun positions -> List.length positions > 0)
      in
      let s = hashfn salt i in
      let new_keys = get_new_keys open_stuff s in
      let acc = acc @ (List.map new_keys ~f:snd |> List.join) in
      let open_stuff =
        filter_open_stuff open_stuff (List.map new_keys ~f:fst)
      in
      let open_stuff =
        match first_run_of_n s 3 with
        | None -> open_stuff
        | Some c ->
            Map.update open_stuff c ~f:(function
              | None -> [ i ]
              | Some stuff -> i :: stuff)
      in
      aux acc open_stuff (i + 1) last_thing
  in
  let res =
    aux [] (Map.empty (module Char)) 0 Int.max_value
    |> List.sort ~compare:Int.compare
  in
  List.take res n

let part1 s =
  let hashfn salt i =
    Md5.digest_string (salt ^ Int.to_string i) |> Md5.to_hex
  in
  find_n_keys s 64 hashfn |> List.last_exn |> Int.to_string |> Ok

let part2 s =
  let hashfn salt i =
    List.fold (List.range 0 2017)
      ~init:(salt ^ Int.to_string i)
      ~f:(fun s _ -> Md5.digest_string s |> Md5.to_hex)
  in
  find_n_keys s 64 hashfn |> List.last_exn |> Int.to_string |> Ok

let%expect_test "" =
  let hashfn salt i =
    List.fold (List.range 0 2017)
      ~init:(salt ^ Int.to_string i)
      ~f:(fun s _ -> Md5.digest_string s |> Md5.to_hex)
  in
  print_s [%sexp (hashfn "abc" 0 : string)];
  [%expect {| a107ff634856bb300138cac6568c0f24 |}];
  ()
