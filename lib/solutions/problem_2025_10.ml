open! Core
open Helpers

let big_num = 1_000_000_000

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

let minimum_presses ?(stop_if_greater_than = big_num) ~counter ~buttons () =
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
       | true -> big_num
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
  aux ~counter ~buttons ~cur_presses:0 ~best_presses:big_num
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

(* i copy and pasted my stuff for part 1 that i wrote a long time ago below *)

module Light : sig
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
