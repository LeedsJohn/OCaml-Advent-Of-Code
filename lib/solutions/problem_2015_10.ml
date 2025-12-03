open! Core

let rle nums =
  let l, n, res =
    List.fold
      (List.tl_exn nums)
      ~init:(1, List.hd_exn nums, [])
      ~f:(fun (l, n, acc) num -> if num = n then l + 1, n, acc else 1, num, n :: l :: acc)
  in
  List.rev (n :: l :: res)
;;

let part1 s =
  let nums = String.to_list s |> List.map ~f:Char.get_digit_exn in
  List.fold (List.range 0 40) ~init:nums ~f:(fun acc _ -> rle acc)
  |> List.length
  |> Int.to_string
;;

let part2 s =
  let nums = String.to_list s |> List.map ~f:Char.get_digit_exn in
  List.fold (List.range 0 50) ~init:nums ~f:(fun acc _ -> rle acc)
  |> List.length
  |> Int.to_string
;;
