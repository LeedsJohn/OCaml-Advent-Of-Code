open! Core
open! Async

let int_to_2_char_string num =
  let s = Int.to_string num in
  if String.length s = 1 then "0" ^ s else s
;;

let get_test_input ~day ~year =
  In_channel.read_all
    [%string "test_input/%{Int.to_string year}/%{int_to_2_char_string day}.txt"]
;;

let get_real_input ~day ~year =
  (* TODO: don't rely on shell commands *)
  let _ = Sys_unix.command [%string "mkdir -p input/%{Int.to_string year}"] in
  let fname = [%string "input/%{Int.to_string year}/%{int_to_2_char_string day}.txt"] in
  match Sys_unix.file_exists_exn fname with
  | true -> return (In_channel.read_all fname)
  | false ->
    let%bind input = Bridge.fetch_real_input ~day ~year in
    Out_channel.write_all fname ~data:input;
    return input
;;

let get_input ~day ~year ~test =
  let%bind input =
    match test with
    | true -> return (get_test_input ~day ~year)
    | false -> get_real_input ~day ~year
  in
  return (String.strip ~drop:(Char.equal '\n') input)
;;
