open! Core

module Stuff = struct
  module T = struct
    type t =
      | Bot of int
      | Output of int
    [@@deriving compare, sexp_of]
  end

  include Comparator.Make (T)
  include T

  let get_num = function
    | Bot n | Output n -> n
  ;;
end

type bot =
  { chips : int list
  ; low_pass : Stuff.t
  ; high_pass : Stuff.t
  }

module Bots = struct
  type t = bot Map.M(Stuff).t

  let add_chip (t : t) ~id ~chip =
    Map.update t id ~f:(function
      | None ->
        { chips = [ chip ]; low_pass = Bot Int.max_value; high_pass = Bot Int.max_value }
      | Some ({ chips; _ } as bot) -> { bot with chips = chip :: chips })
  ;;

  let pass_chip t id =
    let { chips; low_pass; high_pass } = Map.find_exn t id in
    let low, high =
      match List.sort chips ~compare:Int.compare with
      | [ a; b ] -> a, b
      | _ -> raise_s [%message "bad chips" (chips : int list)]
    in
    let t = add_chip t ~id:low_pass ~chip:low in
    let t = add_chip t ~id:high_pass ~chip:high in
    Map.set t ~key:id ~data:{ low_pass; high_pass; chips = [] }
  ;;

  let set_passes t ~id ~low_pass ~high_pass =
    Map.update t id ~f:(function
      | None -> { low_pass; high_pass; chips = [] }
      | Some bot -> { bot with low_pass; high_pass })
  ;;

  let bots_that_need_to_pass t =
    Map.filteri t ~f:(fun ~key:id ~data:{ chips; _ } ->
      match id, List.length chips with
      | Stuff.Bot _, 2 -> true
      | _, _ -> false)
    |> Map.keys
  ;;

  let get_bot_passing t ~low ~high =
    let bot_passing t =
      let m =
        Map.filteri t ~f:(fun ~key:id ~data:{ chips; _ } ->
          match id, chips with
          | Stuff.Bot _, [ a; b ] -> (a = low && b = high) || (a = high && b = low)
          | _, _ -> false)
      in
      if Map.length m = 0 then None else Some (Map.to_alist m |> List.hd_exn |> fst)
    in
    let rec aux t =
      match bot_passing t with
      | Some id -> Stuff.get_num id
      | None ->
        let t = bots_that_need_to_pass t |> List.fold ~init:t ~f:pass_chip in
        aux t
    in
    aux t
  ;;

  let rec run_stuff (t : t) =
    if
      List.for_all [ 0; 1; 2 ] ~f:(fun i ->
        match Map.find t (Stuff.Output i) with
        | None -> false
        | Some { chips; _ } -> List.length chips > 0)
    then t
    else bots_that_need_to_pass t |> List.fold ~init:t ~f:pass_chip |> run_stuff
  ;;

  let get_first_chip t id =
    let { chips; _ } = Map.find_exn t id in
    List.hd_exn chips
  ;;

  let of_string s =
    String.split_lines s
    |> List.fold
         ~init:(Map.empty (module Stuff))
         ~f:(fun acc line ->
           let words = String.split line ~on:' ' |> List.to_array in
           match words.(0) with
           | "value" ->
             add_chip
               acc
               ~id:(Stuff.Bot (Int.of_string words.(5)))
               ~chip:(Int.of_string words.(1))
           | _ ->
             let id = Stuff.Bot (Int.of_string words.(1)) in
             let low_pass =
               let n = Int.of_string words.(6) in
               if String.equal words.(5) "bot" then Stuff.Bot n else Output n
             in
             let high_pass =
               let n = Int.of_string words.(11) in
               if String.equal words.(10) "bot" then Stuff.Bot n else Output n
             in
             set_passes acc ~id ~low_pass ~high_pass)
  ;;
end

let part1 s =
  let bots = Bots.of_string s in
  Bots.get_bot_passing bots ~low:17 ~high:61 |> Int.to_string
;;

let part2 s =
  let bots = Bots.of_string s in
  let bots = Bots.run_stuff bots in
  let n1 = Bots.get_first_chip bots (Stuff.Output 0) in
  let n2 = Bots.get_first_chip bots (Stuff.Output 1) in
  let n3 = Bots.get_first_chip bots (Stuff.Output 2) in
  Int.to_string (n1 * n2 * n3)
;;
