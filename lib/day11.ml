open Core

let day = 11
let year = 2023

let parse_text text =
  String.split_lines text |> List.map ~f:String.to_array |> List.to_array

let get_expansions board empty_row_multiplier =
  let row_expansions = Array.create ~len:(Array.length board) 0 in
  let _ =
    Array.foldi board ~init:0 ~f:(fun row_num prev row ->
        let expansion_count =
          prev
          + (Array.for_all row ~f:(Char.equal '.') |> Bool.to_int)
            * empty_row_multiplier
        in
        row_expansions.(row_num) <- expansion_count;
        expansion_count)
  in
  let col_expansions = Array.create ~len:(Array.length board.(0)) 0 in
  let _ =
    Array.foldi board.(0) ~init:0 ~f:(fun col_num prev _ ->
        let expansion_count =
          prev
          + (Array.for_all board ~f:(fun row -> Char.equal row.(col_num) '.')
            |> Bool.to_int)
            * empty_row_multiplier
        in
        col_expansions.(col_num) <- expansion_count;
        expansion_count)
  in
  (row_expansions, col_expansions)

let get_galaxy_locations board empty_row_multiplier =
  let row_expansions, col_expansions =
    get_expansions board empty_row_multiplier
  in
  Array.foldi board ~init:[] ~f:(fun row_num acc row ->
      acc
      @ Array.foldi row ~init:[] ~f:(fun col_num acc' cell ->
            if Char.equal cell '.' then acc'
            else
              ( col_num + col_expansions.(col_num),
                row_num + row_expansions.(row_num) )
              :: acc'))

let part1 fname =
  let board = parse_text (In_channel.read_all fname) in
  let galaxy_locations = get_galaxy_locations board 1 in
  List.fold galaxy_locations ~init:0 ~f:(fun acc (x1, y1) ->
      acc
      + List.fold galaxy_locations ~init:0 ~f:(fun acc' (x2, y2) ->
            acc' + (Int.abs (x1 - x2) + Int.abs (y1 - y2))))
  / 2
  |> Int.to_string

let part2 fname =
  let board = parse_text (In_channel.read_all fname) in
  let galaxy_locations = get_galaxy_locations board 999999 in
  List.fold galaxy_locations ~init:0 ~f:(fun acc (x1, y1) ->
      acc
      + List.fold galaxy_locations ~init:0 ~f:(fun acc' (x2, y2) ->
            acc' + (Int.abs (x1 - x2) + Int.abs (y1 - y2))))
  / 2
  |> Int.to_string
