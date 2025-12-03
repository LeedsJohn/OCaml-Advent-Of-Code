open! Core

(* i was silly and made a mistake and turned Thing.t into an int (instead of a
   | Chip of string | Generator of string) because I made an error in my bfs and thought
   that I had to make memory saving optimizations when I didn't *)

let get_combos things =
  let rec aux acc = function
    | [ _ ] | [] -> acc
    | hd :: tl -> aux (List.map tl ~f:(fun a -> [ hd; a ]) @ acc) tl
  in
  aux (List.map things ~f:List.return) things
;;

module Thing = struct
  type t = int [@@deriving equal, compare, hash]

  let memo = Hashtbl.create (module String)
  let otherway = Array.create ~len:20 ""
  let stuff = ref 0

  let get_thing ~is_chip ~name =
    let name = if is_chip then "C" ^ name else "G" ^ name in
    match Hashtbl.find memo name with
    | Some res -> res
    | None ->
      let res = !stuff in
      otherway.(res) <- name;
      stuff := !stuff + 1;
      Hashtbl.add_exn memo ~key:name ~data:res;
      res
  ;;

  let is_chip t = Char.equal (String.get otherway.(t) 0) 'C'

  let sexp_of_t t =
    let s = otherway.(t) in
    (match String.get s 0 with
     | 'C' -> "Chip " ^ String.slice s 1 0
     | _ -> "Generator " ^ String.slice s 1 0)
    |> String.sexp_of_t
  ;;

  let toggle t =
    let s = otherway.(t) in
    let name = String.slice s 1 0 in
    match String.get s 0 with
    | 'C' -> get_thing ~is_chip:false ~name
    | _ -> get_thing ~is_chip:true ~name
  ;;
end

module Level = struct
  type t = Thing.t list [@@deriving sexp_of, compare, hash]

  let chip_count t = List.count t ~f:Thing.is_chip

  let valid_level t =
    let count = chip_count t in
    if count <= 1
    then true
    else
      List.for_all t ~f:(fun c ->
        if not (Thing.is_chip c)
        then true
        else List.mem t (Thing.toggle c) ~equal:Thing.equal)
  ;;
end

module Building = struct
  type t =
    { elevator : int
    ; floors : Level.t Map.M(Int).t
    }
  [@@deriving sexp_of, compare, hash]

  let add_thing ({ floors; _ } as t) floor_num thing =
    if not (Int.between floor_num ~low:1 ~high:4)
    then raise_s [%message "trying to add thing to invalid floor" (floor_num : int)];
    let floors =
      Map.update floors floor_num ~f:(function
        | None -> [ thing ]
        | Some things -> List.sort (thing :: things) ~compare:Thing.compare)
    in
    { t with floors }
  ;;

  let move_things { elevator; floors } ~start_floor ~end_floor things =
    if elevator <> start_floor
    then raise_s [%message "elevator <> start_floor" (elevator : int) (start_floor : int)];
    let floors =
      Map.update floors start_floor ~f:(fun floor ->
        Option.value_exn floor
        |> List.filter ~f:(fun c -> not (List.mem things c ~equal:Thing.equal)))
    in
    let floors =
      Map.update floors end_floor ~f:(fun floor ->
        let floor = Option.value_exn floor in
        List.sort (things @ floor) ~compare:Thing.compare)
    in
    { floors; elevator = end_floor }
  ;;

  let of_string s =
    let lines = String.split_lines s in
    List.foldi
      lines
      ~init:
        { elevator = 1
        ; floors = Map.of_alist_exn (module Int) [ 1, []; 2, []; 3, []; 4, [] ]
        }
      ~f:(fun i acc line ->
        let ar = String.split line ~on:' ' |> List.to_array in
        Array.foldi ar ~init:acc ~f:(fun j acc word ->
          match word with
          | "microchip" | "microchip." | "microchip," ->
            let thing = String.chop_suffix_exn ar.(j - 1) ~suffix:"-compatible" in
            add_thing acc (i + 1) (Thing.get_thing ~is_chip:true ~name:thing)
          | "generator" | "generator." | "generator," ->
            add_thing acc (i + 1) (Thing.get_thing ~is_chip:false ~name:ar.(j - 1))
          | _ -> acc))
  ;;

  let valid_building { floors; _ } = Map.for_all floors ~f:Level.valid_level

  let get_all_moves ({ elevator; floors } as t) =
    let floor = Map.find_exn floors elevator in
    let things_to_move = get_combos floor in
    let neighbor_floors =
      List.filter [ elevator - 1; elevator + 1 ] ~f:(Int.between ~low:1 ~high:4)
    in
    List.cartesian_product neighbor_floors things_to_move
    |> List.map ~f:(fun (end_floor, things) ->
      move_things t ~start_floor:elevator ~end_floor things)
    |> List.filter ~f:valid_building
  ;;

  let all_at_top { floors; _ } =
    Map.for_alli floors ~f:(fun ~key:floor_num ~data:stuff ->
      floor_num = 4 || List.length stuff = 0)
  ;;
end

let bfs start =
  let visited = Hash_set.create (module Building) in
  let rec aux cur steps =
    match List.length cur, List.exists cur ~f:Building.all_at_top with
    | 0, _ -> Int.max_value
    | _, true -> steps
    | _, false ->
      (* print_s [%message (cur : Building.t list) (steps : int)]; *)
      let neighbors =
        List.map cur ~f:Building.get_all_moves
        |> List.join
        |> Hash_set.of_list (module Building)
        |> Hash_set.to_list
        |> List.filter ~f:(fun b -> not (Hash_set.mem visited b))
      in
      List.iter neighbors ~f:(Hash_set.add visited);
      aux neighbors (steps + 1)
  in
  Hash_set.add visited start;
  aux [ start ] 0
;;

let part1 s = Building.of_string s |> bfs |> Int.to_string

let part2 s =
  let building = Building.of_string s in
  let building =
    List.fold [ "elerium"; "dilithium" ] ~init:building ~f:(fun acc name ->
      let thing1 = Thing.get_thing ~is_chip:true ~name in
      let thing2 = Thing.get_thing ~is_chip:false ~name in
      let acc = Building.add_thing acc 1 thing1 in
      Building.add_thing acc 1 thing2)
  in
  bfs building |> Int.to_string
;;
