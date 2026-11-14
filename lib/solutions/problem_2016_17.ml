open! Core
open! Helpers

module Room = struct
  let passcode = ref ""

  type t =
    { pos : Coordinate.t
    ; path : string
    }

  let get_neighbors { pos; path } =
    let hash = Md5.digest_string (!passcode ^ path) |> Md5.to_hex in
    let offsets =
      List.filteri
        [ 'U', Coordinate.make ~x:0 ~y:(-1)
        ; 'D', Coordinate.make ~x:0 ~y:1
        ; 'L', Coordinate.make ~x:(-1) ~y:0
        ; 'R', Coordinate.make ~x:1 ~y:0
        ]
        ~f:(fun i (_, offset) ->
          let nx, ny = Coordinate.add pos offset |> Coordinate.xy in
          Int.between nx ~low:0 ~high:3
          && Int.between ny ~low:0 ~high:3
          && Char.between (String.get hash i) ~low:'b' ~high:'f')
    in
    List.map offsets ~f:(fun (c, offset) ->
      { pos = Coordinate.add pos offset; path = path ^ Char.to_string c })
  ;;

  let bfs start goal_pos =
    let rec aux q =
      let ({ pos; path; _ } as cur), q = Fqueue.dequeue_exn q in
      if Coordinate.equal pos goal_pos
      then path
      else
        get_neighbors cur |> List.fold ~init:q ~f:(fun q t -> Fqueue.enqueue q t) |> aux
    in
    aux (Fqueue.singleton start)
  ;;

  let bfs2 start goal_pos =
    let rec aux q =
      match Fqueue.dequeue q with
      | None -> 0
      | Some (({ pos; path; _ } as cur), q) ->
        let score, neighbors =
          if Coordinate.equal pos goal_pos
          then String.length path, []
          else 0, get_neighbors cur
        in
        List.fold neighbors ~init:q ~f:(fun q t -> Fqueue.enqueue q t)
        |> aux
        |> Int.max score
    in
    aux (Fqueue.singleton start)
  ;;
end

let part1 s =
  Room.passcode := s;
  let (start : Room.t) = { pos = Coordinate.origin; path = "" } in
  Room.bfs start (Coordinate.make ~x:3 ~y:3)
;;

let part2 s =
  Room.passcode := s;
  let (start : Room.t) = { pos = Coordinate.origin; path = "" } in
  Room.bfs2 start (Coordinate.make ~x:3 ~y:3) |> Int.to_string
;;
