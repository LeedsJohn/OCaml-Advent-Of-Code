open! Core
open! Helpers

let of_string s = String.split_lines s |> List.map ~f:Parse.line_numbers

let best_score ?(calories_count = Int.max_value) recipes num_ingredients =
  let recipes =
    List.map recipes ~f:(function
      | [ a; b; c; d; calories ] -> ([ a; b; c; d ], calories)
      | recipe -> raise_s [%message "bad recipe" (recipe : int list)])
  in
  let score_stuff stuff calories =
    if calories <> calories_count then 0
    else List.map stuff ~f:(Int.max 0) |> List.fold ~init:1 ~f:Int.( * )
  in
  let rec aux cur calories recipes num_ingredients =
    if num_ingredients = 0 then score_stuff cur calories
    else if calories > calories_count then 0
    else
      match recipes with
      | [] -> score_stuff cur calories
      | (recipe, cals) :: tl ->
          let min_copies = if List.length tl = 0 then num_ingredients else 0 in
          List.range min_copies (num_ingredients + 1)
          |> List.map ~f:(fun i ->
                 let next =
                   List.map2_exn cur recipe ~f:(fun cur n -> cur + (i * n))
                 in
                 aux next (calories + (i * cals)) tl (num_ingredients - i))
          |> List.max_elt ~compare:Int.compare
          |> Option.value_exn
  in
  aux [ 0; 0; 0; 0 ] 0 recipes num_ingredients

let part1 s =
  let recipes = of_string s in
  best_score recipes 100 |> Int.to_string |> Ok

let part2 s =
  let recipes = of_string s in
  best_score ~calories_count:500 recipes 100 |> Int.to_string |> Ok
