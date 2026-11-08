open! Core

module Button : sig
  type t = private int [@@deriving compare, equal, sexp_of]

  include Comparator.S with type t := t

  val parse : string -> t
  val touches_counter : t -> i:int -> bool
  val num_counters : t -> int
  val combine : t -> t -> t
  val empty : t
end = struct
  module T = struct
    type t = int [@@deriving compare, equal, sexp]
  end

  include Comparator.Make (T)
  include T

  let empty = 0
  let touches_counter t ~i = Int.bit_and t (Int.shift_left 1 i) <> 0
  let num_counters = Int.popcount

  let parse s =
    String.chop_prefix_exn s ~prefix:"("
    |> String.chop_suffix_exn ~suffix:")"
    |> String.split ~on:','
    |> List.map ~f:Int.of_string
    |> List.fold ~init:0 ~f:(fun t n -> Int.bit_or t (Int.shift_left 1 n))
  ;;

  let combine = Int.bit_or
end

module Counter : sig
  type t [@@deriving sexp_of]

  val parse : string -> t
  val is_empty : t -> bool
  val press_button : t -> button:Button.t -> times:int -> t
  val max_times_can_press_button : t -> button:Button.t -> int
  val max_count : t -> int

  (*= val min_count : t -> int *)
  val counter_indices : t -> int list
  val counter_index_with_max_count : t -> int
end = struct
  type t = int Map.M(Int).t [@@deriving sexp_of]

  let parse s =
    String.chop_prefix_exn s ~prefix:"{"
    |> String.chop_suffix_exn ~suffix:"}"
    |> String.split ~on:','
    |> List.map ~f:Int.of_string
    |> List.mapi ~f:(fun i n -> i, n)
    |> Map.of_alist_exn (module Int)
  ;;

  let counter_indices = Map.keys

  (*= let min_count t =
    Map.data t |> List.min_elt ~compare:[%compare: int] |> Option.value_exn
  ;; *)

  let max_count t =
    Map.data t |> List.max_elt ~compare:[%compare: int] |> Option.value_exn
  ;;

  let counter_index_with_max_count t =
    let max_count = max_count t in
    Map.to_alist t |> List.find_exn ~f:(fun (_i, count) -> count = max_count) |> fst
  ;;

  let is_empty = Map.for_all ~f:(Int.equal 0)

  let press_button t ~button ~times =
    Map.mapi t ~f:(fun ~key:i ~data:count ->
      match Button.touches_counter button ~i with
      | true -> count - times
      | false -> count)
  ;;

  let max_times_can_press_button t ~button =
    Map.fold t ~init:Int.max_value ~f:(fun ~key:i ~data:count acc ->
      match Button.touches_counter button ~i with
      | true -> Int.min acc count
      | false -> acc)
  ;;
end

let buttons_touching_counter_index ~buttons ~i =
  Set.filter buttons ~f:(Button.touches_counter ~i)
;;

let minimum_presses ?(stop_if_greater_than = 100000) ~counter ~buttons () =
  let rec aux ~counter ~cur_pushes =
    let max_count = Counter.max_count counter in
    match max_count <= 0 || cur_pushes > stop_if_greater_than with
    | true -> cur_pushes
    | false ->
      let i = Counter.counter_index_with_max_count counter in
      let mega_button =
        buttons_touching_counter_index ~buttons ~i
        |> Set.fold ~init:Button.empty ~f:Button.combine
      in
      (match [%equal: Button.t] mega_button Button.empty with
       | true -> cur_pushes
       | false ->
         aux
           ~counter:(Counter.press_button counter ~button:mega_button ~times:max_count)
           ~cur_pushes:(cur_pushes + max_count))
  in
  aux ~counter ~cur_pushes:0
;;

let can_maybe_solve_in_less_than_pushes ~counter ~buttons ~pushes =
  minimum_presses ~stop_if_greater_than:(pushes - 1) ~counter ~buttons () < pushes
;;

let get_button_to_push ~counter ~buttons =
  let best_button_sets =
    Counter.counter_indices counter
    |> List.fold ~init:[ buttons ] ~f:(fun acc i ->
      let buttons_in_group = buttons_touching_counter_index ~buttons ~i in
      let num_buttons = Set.length buttons_in_group in
      match num_buttons > 0 with
      | false -> acc
      | true ->
        (match
           [%compare: int] num_buttons (Set.length (List.hd_exn acc)) |> Ordering.of_int
         with
         | Less -> [ buttons_in_group ]
         | Greater -> acc
         | Equal -> buttons_in_group :: acc))
  in
  (* maybe choose the button that touches the most lights here? or the button with the
       minimum max presses? *)
  let a_button = List.hd_exn best_button_sets |> Set.min_elt_exn in
  match List.hd_exn best_button_sets |> Set.length = 1 with
  | true ->
    let max_presses = Counter.max_times_can_press_button counter ~button:a_button in
    a_button, max_presses, max_presses
  | false ->
    let best_button =
      Set.union_list (module Button) best_button_sets
      |> Set.fold ~init:a_button ~f:(fun best_button button ->
        match Button.num_counters button > Button.num_counters best_button with
        | true -> button
        | false -> best_button)
    in
    best_button, 0, Counter.max_times_can_press_button counter ~button:best_button
;;

let solve ~counter ~buttons =
  let rec aux ~counter ~buttons ~cur_presses ~best_presses =
    match Counter.is_empty counter with
    | true -> cur_presses
    | false ->
      (match
         can_maybe_solve_in_less_than_pushes
           ~counter
           ~buttons
           ~pushes:(best_presses - cur_presses)
       with
       | false -> best_presses
       | true ->
         let button, min_presses, max_presses = get_button_to_push ~counter ~buttons in
         List.range ~stride:(-1) max_presses (min_presses - 1)
         |> List.fold ~init:best_presses ~f:(fun best_presses num_presses ->
           let counter = Counter.press_button counter ~button ~times:num_presses in
           let buttons = Set.remove buttons button in
           aux ~counter ~buttons ~cur_presses:(cur_presses + num_presses) ~best_presses))
  in
  aux ~counter ~buttons ~cur_presses:0 ~best_presses:1000000
;;

let strings_between s ~open_char ~close_char =
  let indices goal_char =
    String.to_list s
    |> List.filter_mapi ~f:(fun i c -> Option.some_if (Char.( = ) c goal_char) i)
  in
  List.zip_exn (indices open_char) (indices close_char)
  |> List.map ~f:(fun (open_i, close_i) ->
    String.sub s ~pos:open_i ~len:(close_i - open_i + 1))
;;

let parse_line s =
  let counter =
    strings_between s ~open_char:'{' ~close_char:'}' |> List.hd_exn |> Counter.parse
  in
  let buttons =
    strings_between s ~open_char:'(' ~close_char:')'
    |> List.map ~f:Button.parse
    |> Set.of_list (module Button)
  in
  counter, buttons
;;

let parse s = String.split_lines s |> List.map ~f:parse_line
let part1 _ = failwith "unimplemented"

let part2 s =
  let i = ref 0 in
  parse s
  |> List.sum
       (module Int)
       ~f:(fun (counter, buttons) ->
         print_s [%sexp (!i : int)];
         i := !i + 1;
         solve ~counter ~buttons)
  |> Int.to_string
;;
(*= module Light : sig
  type t = private int [@@deriving equal]

  val create : string -> t
  val apply_wiring : t -> int -> t
  val of_int : int -> t
  val to_int : t -> int
end = struct
  type t = int [@@deriving equal]

  let create s =
    String.foldi s ~init:0 ~f:(fun i acc c ->
      if Char.equal c '.' then acc else Int.bit_or acc (Int.shift_left 1 i))
  ;;

  let apply_wiring t wiring = Int.bit_xor t wiring
  let to_int = Fn.id
  let of_int = Fn.id
end

module Light_table : sig
  type t = private bool array

  val create : unit -> t
  val copy : t -> t
  val set : t -> Light.t -> unit
  val get : t -> Light.t -> bool
  val iter : t -> f:(Light.t -> unit) -> unit
end = struct
  type t = bool array

  let create () = Array.create ~len:1024 false
  let copy = Array.copy
  let set t light = t.(Light.to_int light) <- true
  let get t light = t.(Light.to_int light)

  let iter t ~f =
    for i = 0 to 1023 do
      if t.(i) then f (Light.of_int i)
    done
  ;;
end

module Wiring : sig
  type t = private int array

  val create : int list list -> t
  val iter : t -> f:(int -> unit) -> unit
end = struct
  type t = int array

  let create nums =
    List.map nums ~f:(fun nums ->
      List.fold nums ~init:0 ~f:(fun acc n -> Int.bit_or acc (Int.shift_left 1 n)))
    |> List.to_array
  ;;

  let iter = Array.iter
end

module Joltage = struct
  type t = private unit

  let bruh : t = Obj.magic ()
end

module Manual = struct
  type t =
    { light : Light.t
    ; wiring : Wiring.t
    ; joltage : Joltage.t
    }

  let parse s =
    let light =
      String.slice s (String.index_exn s '[' + 1) (String.index_exn s ']') |> Light.create
    in
    let wiring =
      List.zip_exn
        (String.substr_index_all s ~may_overlap:false ~pattern:"(")
        (String.substr_index_all s ~may_overlap:false ~pattern:")")
      |> List.map ~f:(fun (i, j) -> String.slice s i j |> Parse.line_numbers)
      |> Wiring.create
    in
    { light; wiring; joltage = Joltage.bruh }
  ;;

  let shortest_path { light; wiring; joltage = _ } =
    let light_table = Light_table.create () in
    Light_table.set light_table (Light.of_int 0);
    let rec bfs res cur =
      if Light_table.get cur light
      then res
      else (
        let next = Light_table.create () in
        Light_table.iter cur ~f:(fun light ->
          Wiring.iter wiring ~f:(fun wire ->
            let nl = Light.apply_wiring light wire in
            Light_table.set light_table nl;
            Light_table.set next nl));
        bfs (res + 1) next)
    in
    bfs 0 (Light_table.copy light_table)
  ;;
end
(* well this shit is pretty worthless for part 2 *)

let parse s = String.split_lines s |> List.map ~f:Manual.parse

let part1 s =
  let manuals = parse s in
  List.sum (module Int) manuals ~f:Manual.shortest_path |> Int.to_string
;;

(* module Counter = struct
  type t = int list [@@deriving compare, equal, hash, sexp]

  (* let valid t goal = List.zip_exn t goal |> List.for_all ~f:(fun (n1, n2) -> n1 <= n2)
  let check_i n i = Int.bit_and (Int.shift_right_logical n i) 1 = 1 *)

  let apply_button t button num_times =
    List.zip_exn t button |> List.map ~f:(fun (x, b) -> x - (b * num_times))
  ;;

  let max_button_pushes t button =
    List.zip_exn t button
    |> List.filter ~f:(fun (_, b) -> b = 1)
    |> List.map ~f:fst
    |> List.min_elt ~compare:Int.compare
    |> Option.value ~default:0
  ;;
end

module Bruh = struct
  type t =
    { counter : Counter.t
    ; buttons : int list list
    ; last_chances : int array
    }

  let parse s =
    let buttons =
      List.zip_exn
        (String.substr_index_all s ~may_overlap:false ~pattern:"(")
        (String.substr_index_all s ~may_overlap:false ~pattern:")")
      |> List.map ~f:(fun (i, j) -> String.slice s i j |> Parse.line_numbers)
    in
    let counter =
      String.slice s (String.index_exn s '{') (String.index_exn s '}')
      |> Parse.line_numbers
    in
    let buttons =
      List.map buttons ~f:(fun buttons ->
        List.init (List.length counter) ~f:(fun i ->
          if List.mem buttons i ~equal:Int.equal then 1 else 0))
      |> List.sort ~compare:(fun b1 b2 ->
        let n1, n2 =
          List.sum (module Int) b1 ~f:Fn.id, List.sum (module Int) b2 ~f:Fn.id
        in
        Int.compare n2 n1)
    in
    let last_chances = Array.create ~len:(List.length counter) (-1) in
    List.iteri buttons ~f:(fun i button ->
      List.iteri button ~f:(fun j n -> if n = 1 then last_chances.(j) <- i));
    { counter; buttons; last_chances }
  ;;

  let solve4 { counter; buttons; last_chances = _ } =
    let last_chance_i buttons button =
      let buttons_with_i = Array.create ~len:(List.length counter) 0 in
      List.iter buttons ~f:(fun button ->
        List.iteri button ~f:(fun i n -> buttons_with_i.(i) <- buttons_with_i.(i) + n));
      List.findi button ~f:(fun i n -> n = 1 && buttons_with_i.(i) = 1)
      |> Option.map ~f:fst
    in
    let num_presses_to_try ~cur_presses ~best ~counter ~buttons ~button =
      let biggest = Counter.max_button_pushes counter button in
      match last_chance_i buttons button with
      | Some n -> [ List.nth_exn counter n ]
      | None ->
        if List.length buttons = 1
        then [ biggest ]
        else if
          cur_presses + (List.max_elt counter ~compare:Int.compare |> Option.value_exn)
          >= best
        then []
        else List.range ~stride:(-1) (Int.min (best - cur_presses) biggest) (-1)
    in
    let next_button ~cur_presses ~best ~counter ~buttons =
      List.fold
        (List.tl_exn buttons)
        ~init:
          ( num_presses_to_try
              ~cur_presses
              ~best
              ~counter
              ~buttons
              ~button:(List.hd_exn buttons)
          , List.hd_exn buttons )
        ~f:(fun ((thingy, _) as acc) button ->
          let a = num_presses_to_try ~cur_presses ~best ~counter ~buttons ~button in
          if List.length a < List.length thingy then a, button else acc)
    in
    let rec aux ~i ~cur_presses ~best counter = function
      | [] -> if List.for_all counter ~f:(Int.equal 0) then cur_presses else Int.max_value
      | buttons ->
        if cur_presses >= best || List.exists counter ~f:(fun n -> n < 0)
        then Int.max_value
        else (
          let john, button = next_button ~cur_presses ~best ~counter ~buttons in
          let next_buttons =
            List.filter buttons ~f:(fun b -> not ([%equal: int list] b button))
          in
          List.fold john ~init:best ~f:(fun best num_times ->
            Int.min
              best
              (aux
                 ~i:(i + 1)
                 ~cur_presses:(cur_presses + num_times)
                 ~best
                 (Counter.apply_button counter button num_times)
                 next_buttons)))
    in
    aux ~i:0 ~cur_presses:0 ~best:Int.max_value counter buttons
  ;;

  let _solve3 { counter; buttons; last_chances } =
    let next_button counter buttons =
      let buttons_with_i = Array.create ~len:(List.length counter) 0 in
      List.iter buttons ~f:(fun button ->
        List.iteri button ~f:(fun i n -> buttons_with_i.(i) <- buttons_with_i.(i) + n));
      let get_score button =
        let thing =
          List.filter_mapi button ~f:(fun i n ->
            if n = 0 || buttons_with_i.(i) = 0 then None else Some buttons_with_i.(i))
          |> List.sort_and_group ~compare:Int.compare
        in
        let a = List.hd_exn thing in
        (-10000 * List.hd_exn a) - List.length a
      in
      List.fold
        buttons
        ~init:(Int.max_value, List.hd_exn buttons)
        ~f:(fun (best, bruh) b ->
          let score = get_score b in
          if score < best then score, b else best, bruh)
      |> snd
    in
    let num_presses_to_try ~i ~cur_presses ~best ~counter ~button =
      let biggest = Counter.max_button_pushes counter button in
      let last = Array.findi last_chances ~f:(fun _j k -> k = i) |> Option.map ~f:fst in
      match last with
      | Some n -> [ List.nth_exn counter n ]
      | None ->
        if i + 1 = List.length buttons
        then [ biggest ]
        else if
          cur_presses + (List.max_elt counter ~compare:Int.compare |> Option.value_exn)
          >= best
        then []
        else List.range ~stride:(-1) (Int.min (best - cur_presses) biggest) (-1)
    in
    let rec aux ~i ~cur_presses ~best counter = function
      | [] -> if List.for_all counter ~f:(Int.equal 0) then cur_presses else Int.max_value
      | buttons ->
        if cur_presses >= best || List.exists counter ~f:(fun n -> n < 0)
        then Int.max_value
        else (
          let button = next_button counter buttons in
          let next_buttons =
            List.filter buttons ~f:(fun b -> not ([%equal: int list] b button))
          in
          num_presses_to_try ~i ~cur_presses ~best ~counter ~button
          |> List.fold ~init:best ~f:(fun best num_times ->
            Int.min
              best
              (aux
                 ~i:(i + 1)
                 ~cur_presses:(cur_presses + num_times)
                 ~best
                 (Counter.apply_button counter button num_times)
                 next_buttons)))
    in
    aux ~i:0 ~cur_presses:0 ~best:Int.max_value counter buttons
  ;;

  (* this is really stupid and i could make it faster (or just use a linear solver) 
     but whatever *)
  let _solve2 { counter; buttons; last_chances } =
    let num_presses_to_try ~i ~cur_presses ~best ~counter ~button =
      let biggest = Counter.max_button_pushes counter button in
      let last = Array.findi last_chances ~f:(fun _j k -> k = i) |> Option.map ~f:fst in
      match last with
      | Some n -> [ List.nth_exn counter n ]
      | None ->
        if i + 1 = List.length buttons
        then [ biggest ]
        else if
          cur_presses + (List.max_elt counter ~compare:Int.compare |> Option.value_exn)
          >= best
        then []
        else List.range ~stride:(-1) (Int.min (best - cur_presses) biggest) (-1)
    in
    let rec aux ~i ~cur_presses ~best counter = function
      | [] -> if List.for_all counter ~f:(Int.equal 0) then cur_presses else Int.max_value
      | button :: tl ->
        if cur_presses >= best || List.exists counter ~f:(fun n -> n < 0)
        then Int.max_value
        else
          num_presses_to_try ~i ~cur_presses ~best ~counter ~button
          |> List.fold ~init:best ~f:(fun best num_times ->
            Int.min
              best
              (aux
                 ~i:(i + 1)
                 ~cur_presses:(cur_presses + num_times)
                 ~best
                 (Counter.apply_button counter button num_times)
                 tl))
    in
    aux ~i:0 ~cur_presses:0 ~best:Int.max_value counter buttons
  ;;

  (* let solve { counter = goal; buttons } =
    let start = List.init (List.length goal) ~f:(fun _ -> 0) in
    let visited = Hash_set.create (module Counter) in
    Hash_set.add visited start;
    let rec bfs res cur =
      if Hash_set.mem visited goal
      then res
      else (
        let neighbors =
          List.map cur ~f:(fun counter ->
            List.map buttons ~f:(Counter.apply_button counter))
          |> List.join
          |> List.filter ~f:(fun counter ->
            if (not (Counter.valid counter goal)) || Hash_set.mem visited counter
            then false
            else (
              Hash_set.add visited counter;
              true))
        in
        bfs (res + 1) neighbors)
    in
    bfs 0 [ start ]
  ;; *)
end

let parse2 s = String.split_lines s |> List.map ~f:Bruh.parse *)

module Button = struct
  type t = int

  let get (t : t) ~counter_index = Int.bit_and t (Int.shift_left 1 counter_index) <> 0
end

let num_buttons_with_counter_index buttons counter_index =
  List.count buttons ~f:(fun button -> Button.get button ~counter_index)
;;

let can_decrement_counter_index buttons counter_index =
  num_buttons_with_counter_index buttons counter_index > 0
;;

module Counter2 = struct
  type t = int list

  let push_button (t : t) button ~num_pushes =
    List.mapi t ~f:(fun counter_index n ->
      if Button.get button ~counter_index then n - num_pushes else n)
  ;;

  let is_possible t buttons ~max_remaining_presses =
    if
      not
        (List.for_alli t ~f:(fun counter_index n ->
           n <= max_remaining_presses
           && (n = 0 || can_decrement_counter_index buttons counter_index)))
    then false
    else if max_remaining_presses > 100000
    then true
    else (
      let most_things =
        List.fold buttons ~init:0 ~f:(fun acc b -> Int.max acc (Int.popcount b))
      in
      most_things * max_remaining_presses >= List.sum (module Int) t ~f:Fn.id)
  ;;

  let is_solved t = List.for_all t ~f:(Int.equal 0)
end

let presses_to_try ~button ~all_buttons ~counter ~max_remaining_presses =
  let max_presses =
    Int.min
      max_remaining_presses
      (List.filteri counter ~f:(fun i _ -> Button.get button ~counter_index:i)
       |> List.min_elt ~compare:Int.compare
       |> Option.value_exn)
  in
  let min_presses =
    let counter_index_only_this_button =
      List.range 0 (List.length counter)
      |> List.filter ~f:(fun counter_index ->
        Button.get button ~counter_index
        && num_buttons_with_counter_index all_buttons counter_index = 1)
    in
    match counter_index_only_this_button with
    | [] -> 0
    | hd :: _ -> List.nth_exn counter hd
  in
  if min_presses > max_presses
  then []
  else if min_presses <> 0
  then [ min_presses ]
  else List.range ~stride:(-1) max_presses (min_presses - 1)
;;

let next_button buttons counter =
    let zero_press_button = List.find buttons ~f:(fun button ->
        List.existsi counter ~f:(fun counter_index c ->
            c = 0 && Button.get button ~counter_index <> 0

        )
    )
    in
    match zero_press_button with
    | Some b -> b
    | None -> (
        let button_counter_index_counts = Array.init (List.length counter) ~f:(fun counter_index ->
            num_buttons_with_counter_index buttons counter_index
        )
        in
        List.range 1 (List.length buttons) 
    |> List.find_map ~f:(fun num_buttons ->

            )
    )

let next_button_and_stuff_to_try buttons ~counter ~max_remaining_presses =
  List.fold
    (List.tl_exn buttons)
    ~init:
      ( List.hd_exn buttons
      , presses_to_try
          ~button:(List.hd_exn buttons)
          ~all_buttons:buttons
          ~counter
          ~max_remaining_presses )
    ~f:(fun ((_, prev) as acc) button ->
      let p =
        presses_to_try ~button ~all_buttons:buttons ~counter ~max_remaining_presses
      in
      if List.length p < List.length prev then button, p else acc)
;;

let solve counter buttons =
  let rec aux ~cur_presses ~best ~counter ~buttons =
    let max_remaining_presses = best - cur_presses in
    if not (Counter2.is_possible counter buttons ~max_remaining_presses)
    then best
    else if List.length buttons = 0
    then if Counter2.is_solved counter then cur_presses else best
    else (
      let button, presses =
        next_button_and_stuff_to_try buttons ~counter ~max_remaining_presses
      in
      let new_buttons = List.filter buttons ~f:(fun b -> not (b = button)) in
      List.fold presses ~init:best ~f:(fun res num_pushes ->
        aux
          ~cur_presses:(cur_presses + num_pushes)
          ~best:res
          ~counter:(Counter2.push_button counter button ~num_pushes)
          ~buttons:new_buttons))
  in
  aux ~cur_presses:0 ~best:Int.max_value ~counter ~buttons
;;

let parse3 s =
  String.split_lines s
  |> List.map ~f:(fun s ->
    let buttons =
      List.zip_exn
        (String.substr_index_all s ~may_overlap:false ~pattern:"(")
        (String.substr_index_all s ~may_overlap:false ~pattern:")")
      |> List.map ~f:(fun (i, j) ->
        String.slice s i j
        |> Parse.line_numbers
        |> List.fold ~init:0 ~f:(fun acc n -> Int.bit_or acc (Int.shift_left 1 n)))
    in
    let counter =
      String.slice s (String.index_exn s '{') (String.index_exn s '}')
      |> Parse.line_numbers
    in
    counter, buttons)
;;

let part2 s =
  let stuff = parse3 s in
  List.mapi stuff ~f:(fun i (counter, buttons) ->
    print_s [%sexp (i : int)];
    solve counter buttons)
  |> List.sum (module Int) ~f:Fn.id
  |> Int.to_string
;; *)
