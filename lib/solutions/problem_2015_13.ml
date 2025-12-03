open! Core

let permutations l =
  let rec all_combos acc before after : ('a * 'a list) list =
    match after with
    | [] -> acc
    | hd :: tl -> all_combos ((hd, before @ tl) :: acc) (hd :: before) tl
  in
  let rec aux (l : 'a list) : 'a list list =
    match l with
    | [] -> []
    | [ hd ] -> [ [ hd ] ]
    | l ->
      all_combos [] [] l
      |> List.map ~f:(fun (n, l) ->
        aux l |> List.map ~f:(fun other_perms -> n :: other_perms))
      |> List.join
  in
  aux l
;;

module Stuff = struct
  type t = char * char [@@deriving sexp_of, compare, hash]

  let make c1 c2 = if Char.( < ) c1 c2 then c1, c2 else c2, c1
end

module People = struct
  type t = int Hashtbl.M(Stuff).t

  let add (t : t) p1 p2 num =
    let key = Stuff.make p1 p2 in
    Hashtbl.update t key ~f:(function
      | None -> num
      | Some n -> n + num)
  ;;

  let get t p1 p2 = Hashtbl.find t (Stuff.make p1 p2) |> Option.value ~default:0

  let of_string s =
    let stuff = Hashtbl.create (module Stuff) in
    String.split_lines s
    |> List.map ~f:(fun line ->
      let line = String.split line ~on:' ' |> List.to_array in
      let p1 = String.get line.(0) 0 in
      let p2 = String.get line.(10) 0 in
      let n = Int.of_string line.(3) in
      let n = if String.equal line.(2) "gain" then n else -n in
      p1, p2, n)
    |> List.iter ~f:(fun (p1, p2, n) -> add stuff p1 p2 n);
    stuff
  ;;
end

let score_setup edges people =
  let first = List.hd_exn people in
  let rec aux acc = function
    | [] -> acc
    | [ hd ] -> People.get edges first hd + acc
    | a :: b :: tl -> aux (acc + People.get edges a b) (b :: tl)
  in
  aux 0 people
;;

let part1 s =
  let edges = People.of_string s in
  let people =
    Hashtbl.fold
      edges
      ~init:(Set.empty (module Char))
      ~f:(fun ~key:(p1, p2) ~data:_ acc ->
        let acc = Set.add acc p1 in
        Set.add acc p2)
    |> Set.to_list
  in
  permutations people
  |> List.map ~f:(score_setup edges)
  |> List.max_elt ~compare:Int.compare
  |> Option.value_exn
  |> Int.to_string
;;

let part2 s =
  let edges = People.of_string s in
  let people =
    Hashtbl.fold
      edges
      ~init:(Set.singleton (module Char) ' ')
      ~f:(fun ~key:(p1, p2) ~data:_ acc ->
        let acc = Set.add acc p1 in
        Set.add acc p2)
    |> Set.to_list
  in
  permutations people
  |> List.map ~f:(score_setup edges)
  |> List.max_elt ~compare:Int.compare
  |> Option.value_exn
  |> Int.to_string
;;

let%expect_test "" =
  print_s [%sexp (permutations [ 1; 2; 3 ] : int list list)];
  [%expect {| ((3 1 2) (3 2 1) (2 3 1) (2 1 3) (1 3 2) (1 2 3)) |}];
  ()
;;
