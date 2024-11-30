open! Core
open! Async

(* Functions to make requests to the AOC website. not sure what a better name for this
   module would have been *)

let session_cookie = In_channel.read_all "session_cookie.txt" |> String.strip

let fetch_real_input ~day ~year =
  let headers = Cohttp.Header.of_list [ ("Cookie", session_cookie) ] in
  let uri =
    Uri.of_string
      [%string
        "https://adventofcode.com/%{Int.to_string year}/day/%{Int.to_string \
         day}/input"]
  in
  let%bind _res, body = Cohttp_async.Client.get ~headers uri in
  Cohttp_async.Body.to_string body

module Submission_result = struct
  type t = Correct | Already_submitted | Incorrect [@@deriving sexp]

  let of_body body =
    if String.is_substring body ~substring:"That's the right answer!" then
      Correct
    else if
      String.is_substring body
        ~substring:
          "You don't seem to be solving the right level.  Did you already \
           complete it?"
    then Already_submitted
    else Incorrect
end

let submit_solution ~day ~year ~part ~answer =
  let headers =
    Cohttp.Header.of_list
      [
        ("Cookie", session_cookie);
        ("Content-Type", "application/x-www-form-urlencoded");
      ]
  in
  let body =
    Cohttp_async.Body.of_string
      [%string "level=%{Int.to_string part}&answer=%{answer}"]
  in
  let uri =
    Uri.of_string
      [%string
        "https://adventofcode.com/%{Int.to_string year}/day/%{Int.to_string \
         day}/answer"]
  in
  let%bind _res, body = Cohttp_async.Client.post ~headers ~body uri in
  let%bind body = Cohttp_async.Body.to_string body in
  return (Submission_result.of_body body)
