open! Core

let shift_letter c n =
  let num = Char.to_int c - Char.to_int 'a' in
  let num = (num + n) % 26 in
  Char.of_int_exn (num + Char.to_int 'a')

module Room = struct
  type t = { name : string; id : int; checksum : string } [@@deriving sexp]

  let of_string s =
    let n = String.length s in
    let checksum = String.slice s (n - 6) (n - 1) in
    let i =
      String.rfindi s ~f:(fun _ c -> Char.equal '-' c) |> Option.value_exn
    in
    let id = String.slice s (i + 1) (n - 7) |> Int.of_string in
    let name = String.slice s 0 i in
    { name; id; checksum }

  let expected_checksum { name; _ } =
    let nums =
      String.to_list "abcdefghijklmnopqrstuvwxyz"
      |> List.map ~f:(fun c -> (c, String.count name ~f:(Char.equal c)))
      |> List.sort ~compare:(fun (c1, n1) (c2, n2) ->
             if n1 <> n2 then Int.compare n2 n1 else Char.compare c1 c2)
      |> List.map ~f:fst
    in
    List.take nums 5 |> String.of_list

  let decode { name; id; checksum } =
    let name =
      String.map name ~f:(fun c ->
          if Char.equal c '-' then '-' else shift_letter c id)
    in
    { name; id; checksum }
end

let part1 s =
  String.split_lines s |> List.map ~f:Room.of_string
  |> List.sum
       (module Int)
       ~f:(fun ({ id; checksum; _ } as t) ->
         if String.equal checksum (Room.expected_checksum t) then id else 0)
  |> Int.to_string |> Ok

let part2 s =
  String.split_lines s |> List.map ~f:Room.of_string |> List.map ~f:Room.decode
  |> List.find_map_exn ~f:(fun { name; id; _ } ->
         if String.is_substring name ~substring:"northpole" then Some id
         else None)
  |> Int.to_string |> Ok

let%expect_test "" =
  print_s [%sexp (Room.of_string "aaaaa-bbb-z-y-x-123[abxyz]" : Room.t)];
  [%expect {| ((name aaaaa-bbb-z-y-x) (id 123) (checksum abxyz)) |}];
  ()

let%expect_test "shift letter" =
  print_s [%sexp (shift_letter 'a' 27 : char)];
  [%expect {| b |}];
  ()
