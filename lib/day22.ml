open Core

let day = 22
let year = 2023

type cube = Empty | Cube of int

let get_cubes lines =
  let get_points text =
    let nums = String.split text ~on:',' |> List.map ~f:Int.of_string in
    (List.nth_exn nums 0, List.nth_exn nums 1, List.nth_exn nums 2)
  in

  List.map lines ~f:(fun line ->
      let line = String.split line ~on:'~' in
      (get_points (List.nth_exn line 0), get_points (List.nth_exn line 1)))

let get_max_xyz cubes =
  let three_max a b c = Int.max a b |> Int.max c in
  List.fold cubes ~init:(0, 0, 0)
    ~f:(fun (x, y, z) ((x1, y1, z1), (x2, y2, z2)) ->
      (three_max x x1 x2, three_max y y1 y2, three_max z z1 z2))

module Int_hashtbl = Hashtbl.Make (Int)

module ThreeDarray = struct
  type t = {
    board : cube array array array;
    bricks : ((int * int * int) * (int * int * int)) Int_hashtbl.t;
  }

  let copy t =
    let new_board =
      Array.init (Array.length t.board) ~f:(fun x ->
          Array.init
            (Array.length t.board.(0))
            ~f:(fun y ->
              Array.init
                (Array.length t.board.(0).(0))
                ~f:(fun z -> t.board.(x).(y).(z))))
    in
    let new_bricks = Hashtbl.copy t.bricks in
    { board = new_board; bricks = new_bricks }

  let get_cube_coordinates ((x1, y1, z1), (x2, y2, z2)) =
    let get_range a b = List.range (Int.min a b) (Int.max a b + 1) in
    List.fold (get_range x1 x2) ~init:[] ~f:(fun acc x ->
        List.fold (get_range y1 y2) ~init:acc ~f:(fun acc y ->
            List.fold (get_range z1 z2) ~init:acc ~f:(fun acc z ->
                (x, y, z) :: acc)))

  let make brick_list =
    let res = Int_hashtbl.create () in
    let max_x, max_y, max_z = get_max_xyz brick_list in
    (* I spent like 90 minutes debugging because I originally wrote
       let board = Array.make_matrix (dimensions) (Array.create ...)
       And this resulted in there being a bunch of copies of the same array
       rather than different arrays *)
    let board =
      Array.init (max_x + 1) ~f:(fun _ ->
          Array.init (max_y + 1) ~f:(fun _ ->
              Array.init (max_z + 1) ~f:(fun _ -> Empty)))
    in
    let add_brick id brick =
      List.iter (get_cube_coordinates brick) ~f:(fun (x, y, z) ->
          board.(x).(y).(z) <- Cube id);
      Hashtbl.add_exn res ~key:id ~data:brick
    in
    List.iteri brick_list ~f:(fun i brick -> add_brick i brick);
    { board; bricks = res }

  let get_bricks_under t brick_id =
    let brick = Hashtbl.find_exn t.bricks brick_id in
    List.fold
      (get_cube_coordinates brick)
      ~init:(Set.empty (module Int))
      ~f:(fun acc (x, y, z) ->
        match t.board.(x).(y).(z - 1) with
        | Empty -> acc
        | Cube id -> if brick_id <> id then Set.add acc id else acc)

  let can_shuffle_down t brick_id =
    let brick = Hashtbl.find_exn t.bricks brick_id in
    List.for_all (get_cube_coordinates brick) ~f:(fun (x, y, z) ->
        z > 1
        &&
        match t.board.(x).(y).(z - 1) with
        | Empty -> true
        | Cube id -> brick_id = id)

  let move_brick_down_once t brick_id =
    let brick = Hashtbl.find_exn t.bricks brick_id in
    let cube_positions =
      get_cube_coordinates brick
      |> List.sort ~compare:(fun (_, _, z1) (_, _, z2) -> Int.compare z1 z2)
    in
    List.iter cube_positions ~f:(fun (x, y, z) ->
        t.board.(x).(y).(z - 1) <- t.board.(x).(y).(z);
        t.board.(x).(y).(z) <- Empty);
    Hashtbl.update t.bricks brick_id ~f:(fun _ ->
        let (x1, y1, z1), (x2, y2, z2) = brick in
        ((x1, y1, z1 - 1), (x2, y2, z2 - 1)))

  let move_brick_down t brick_id =
    while can_shuffle_down t brick_id do
      move_brick_down_once t brick_id
    done

  let move_bricks_down_once t =
    List.fold (Hashtbl.keys t.bricks) ~init:false ~f:(fun acc brick_id ->
        if can_shuffle_down t brick_id then (
          move_brick_down t brick_id;
          true)
        else acc)

  let move_bricks_down t =
    while move_bricks_down_once t do
      ()
    done

  let get_useless_bricks t =
    let all_brick_ids = Hashtbl.keys t.bricks |> Set.of_list (module Int) in
    let useless_brick_ids =
      Hashtbl.fold t.bricks ~init:all_brick_ids
        ~f:(fun ~key:brick_id ~data:_ acc ->
          let bricks_under = get_bricks_under t brick_id in
          if Set.length bricks_under = 1 then
            Set.remove acc (Set.choose_exn bricks_under)
          else acc)
    in
    Set.length useless_brick_ids

  let remove_brick t brick_id =
    let brick = Hashtbl.find_exn t.bricks brick_id in
    List.iter (get_cube_coordinates brick) ~f:(fun (x, y, z) ->
        t.board.(x).(y).(z) <- Empty);
    Hashtbl.remove t.bricks brick_id

  let count_num_falling_if_removed t brick_id =
    let new_t = copy t in
    remove_brick new_t brick_id;
    move_bricks_down new_t;
    List.count (Hashtbl.to_alist t.bricks)
      ~f:(fun (brick_id, ((_, _, z1), _)) ->
        match Hashtbl.find new_t.bricks brick_id with
        | None -> false
        | Some ((_, _, z2), _) -> z1 <> z2)

  let count_falling_stuff t =
    List.fold (Hashtbl.keys t.bricks) ~init:0 ~f:(fun acc brick_id ->
        acc + count_num_falling_if_removed t brick_id)

  let _print_board t =
    let get_s x y z =
      match t.board.(x).(y).(z) with Empty -> "" | Cube id -> Int.to_string id
    in
    List.iter
      (List.range 0 (Array.length t.board.(0).(0)))
      ~f:(fun z ->
        List.iter
          (List.range 0 (Array.length t.board.(0)))
          ~f:(fun y ->
            List.iter
              (List.range 0 (Array.length t.board))
              ~f:(fun x -> printf "(%d, %d, %d): %s\n" x y z (get_s x y z))))
end

let parse_text text =
  let text = String.strip text |> String.split_lines in
  let cube_list = get_cubes text in
  ThreeDarray.make cube_list

let part1 fname =
  let t = parse_text (In_channel.read_all fname) in
  ThreeDarray.move_bricks_down t;
  ThreeDarray.get_useless_bricks t |> Int.to_string

let part2 fname =
  let t = parse_text (In_channel.read_all fname) in
  ThreeDarray.move_bricks_down t;
  ThreeDarray.count_falling_stuff t |> Int.to_string
