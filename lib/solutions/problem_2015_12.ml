open! Core
open! Helpers

module Json = struct
  type t =
    | Ar of t list
    | Ob of t Map.M(String).t
    | S of string
    | N of int
  [@@deriving sexp_of, equal]

  let of_string s =
    let rec get_thing i =
      match String.get s i with
      | '{' -> get_obj (Map.empty (module String)) (i + 1)
      | '[' -> get_array [] (i + 1)
      | '"' -> get_str (i + 1)
      | c ->
        if Char.(c = '-' || is_digit c)
        then get_num i
        else raise_s [%message "idk how to do this" (c : char)]
    and get_str i =
      let next_quote = String.substr_index_exn ~pos:(i + 1) s ~pattern:"\"" in
      S (String.slice s i next_quote), next_quote + 1
    and get_num i =
      let end_i = ref i in
      while
        let c = String.get s !end_i in
        Char.(c = '-' || is_digit c)
      do
        end_i := !end_i + 1
      done;
      N (Int.of_string (String.slice s i !end_i)), !end_i
    and get_array (acc : t list) i =
      match String.get s i with
      | ']' -> Ar acc, i + 1
      | ',' -> get_array acc (i + 1)
      | _ ->
        let new_thing, i = get_thing i in
        get_array (new_thing :: acc) i
    and get_obj acc i =
      match String.get s i with
      | '}' -> Ob acc, i + 1
      | '"' ->
        let j = String.substr_index_exn ~pos:(i + 1) s ~pattern:"\"" in
        let key = String.slice s (i + 1) j in
        let data, i = get_thing (j + 2) in
        get_obj (Map.add_exn acc ~key ~data) i
      | ',' -> get_obj acc (i + 1)
      | c ->
        raise_s
          [%message
            "malformed object"
              (c : char)
              ~obj:(acc : t Map.M(String).t)
              ~remaining_string:(String.slice s i 0 : string)]
    in
    fst (get_thing 0)
  ;;

  let rec get_stuff t =
    match t with
    | N n -> n
    | S _ -> 0
    | Ar a -> List.sum (module Int) a ~f:get_stuff
    | Ob m ->
      if List.mem (Map.data m) (S "red") ~equal
      then 0
      else Map.sum (module Int) m ~f:get_stuff
  ;;
end

let part1 s =
  String.split_lines s
  |> List.sum
       (module Int)
       ~f:(fun line -> Parse.line_numbers line |> List.sum (module Int) ~f:Fn.id)
  |> Int.to_string
;;

let part2 s =
  let t = Json.of_string s in
  Json.get_stuff t |> Int.to_string
;;

let%expect_test "" =
  print_s [%sexp (Json.of_string "{}" : Json.t)];
  [%expect {| (Ob ()) |}];
  print_s [%sexp (Json.of_string "\"john\"" : Json.t)];
  [%expect {| (S john) |}];
  print_s [%sexp (Json.of_string "{\"c\":\"red\"}" : Json.t)];
  [%expect {| (Ob ((c (S red)))) |}];
  ()
;;
