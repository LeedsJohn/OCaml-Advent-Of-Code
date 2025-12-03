open! Core

let supports_tls s =
  let is_abba i =
    if i + 3 >= String.length s
    then false
    else (
      let a, b, c, d =
        String.get s i, String.get s (i + 1), String.get s (i + 2), String.get s (i + 3)
      in
      Char.(a = d && b = c && a <> b))
  in
  let in_subnet = ref false in
  let found_abba = ref false in
  let abba_in_subnet = ref false in
  for i = 0 to String.length s - 1 do
    if !in_subnet
    then (
      if Char.equal (String.get s i) ']' then in_subnet := false;
      if is_abba i then abba_in_subnet := true)
    else (
      if is_abba i then found_abba := true;
      if Char.equal (String.get s i) '[' then in_subnet := true)
  done;
  !found_abba && not !abba_in_subnet
;;

let supports_ssl s =
  let subnet_indices =
    List.range 0 (String.length s)
    |> List.filter ~f:(fun i ->
      let c = String.get s i in
      Char.(c = '[' || c = ']'))
  in
  let rec get_subnets acc = function
    | [] -> acc
    | a :: b :: tl -> get_subnets (String.slice s (a + 1) b :: acc) tl
    | _ -> raise_s [%message "wtf"]
  in
  let subnets = get_subnets [] subnet_indices in
  let subnets_contains substring =
    List.exists subnets ~f:(String.is_substring ~substring)
  in
  let in_subnet = ref false in
  String.existsi (String.drop_suffix s 2) ~f:(fun i a ->
    if !in_subnet
    then (
      if Char.equal a ']' then in_subnet := false;
      false)
    else if Char.equal a '['
    then (
      in_subnet := true;
      false)
    else (
      let b, c = String.get s (i + 1), String.get s (i + 2) in
      Char.(a = c && a <> b && b <> ']' && b <> '[')
      && subnets_contains (String.of_list [ b; a; b ])))
;;

let part1 s = String.split_lines s |> List.count ~f:supports_tls |> Int.to_string
let part2 s = String.split_lines s |> List.count ~f:supports_ssl |> Int.to_string
