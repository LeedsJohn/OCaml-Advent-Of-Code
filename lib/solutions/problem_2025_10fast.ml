open! Core

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
  val fold_counter_indices : t -> init:'acc -> f:('acc -> int -> 'acc) -> 'acc
  val counter_index_with_max_count : t -> int
  val count_at : t -> i:int -> int
end = struct
  type t = int array [@@deriving sexp_of]

  let count_at t ~i = t.(i)

  let parse s =
    String.chop_prefix_exn s ~prefix:"{"
    |> String.chop_suffix_exn ~suffix:"}"
    |> String.split ~on:','
    |> List.map ~f:Int.of_string
    |> Array.of_list
  ;;

  let rec fold_counter_indices' t acc i ~f =
    match i >= Array.length t with
    | true -> acc
    | false -> fold_counter_indices' t (f acc i) (i + 1) ~f
  ;;

  let fold_counter_indices t ~init ~f = fold_counter_indices' t init 0 ~f
  let max_count t = Array.max_elt t ~compare:[%compare: int] |> Option.value_exn

  let counter_index_with_max_count t =
    let max_count = max_count t in
    Array.findi_exn t ~f:(fun _ n -> Int.( = ) max_count n) |> fst
  ;;

  let is_empty = Array.for_all ~f:(Int.( = ) 0)

  let press_button t ~button ~times =
    Array.init (Array.length t) ~f:(fun i ->
      match Button.touches_counter button ~i with
      | true -> t.(i) - times
      | false -> t.(i))
  ;;

  let max_times_can_press_button t ~button =
    Array.foldi t ~init:big_num ~f:(fun i acc count ->
      match Button.touches_counter button ~i with
      | true -> Int.min acc count
      | false -> acc)
  ;;
end

let strings_between s ~open_char ~close_char =
  let indices goal_char =
    String.to_list s
    |> List.filter_mapi ~f:(fun i c -> Option.some_if (Char.( = ) c goal_char) i)
  in
  List.zip_exn (indices open_char) (indices close_char)
  |> List.map ~f:(fun (open_i, close_i) ->
    String.sub s ~pos:open_i ~len:(close_i - open_i + 1))
;;

module Button_set : sig
  type t

  val of_string : string -> t
  val touching_counter_index : t -> i:int -> t
  val remove : t -> button:Button.t -> t
  val fold : t -> init:'acc -> f:('acc -> Button.t -> 'acc) -> 'acc
  val length : t -> int
  val button_with_most_lights : t -> Button.t

  (** Returns some button in the set. Raises if there are no buttons. *)
  val get_button : t -> Button.t
end = struct
  type t =
    { buttons : Button.t array
    ; length : int
    }

  let of_string s =
    let buttons =
      strings_between s ~open_char:'(' ~close_char:')'
      |> List.map ~f:Button.parse
      |> Array.of_list
    in
    { buttons; length = Array.length buttons }
  ;;

  let get_button { buttons; length } =
    if length = 0 then failwith "no buttons in set";
    buttons.(0)
  ;;

  let length { length; buttons = _ } = length

  let touching_counter_index { buttons; length = start_length } ~i =
    let rec aux ~l ~r ~length =
      match l > r with
      | true -> length
      | false ->
        (match Button.touches_counter buttons.(l) ~i with
         | true -> aux ~l:(l + 1) ~r ~length:(length + 1)
         | false ->
           Array.swap buttons l r;
           aux ~l ~r:(r - 1) ~length)
    in
    let length = aux ~l:0 ~r:(start_length - 1) ~length:0 in
    { buttons; length }
  ;;

  let remove { buttons; length } ~button =
    let i = Array.findi_exn buttons ~f:(fun _ b -> [%equal: Button.t] b button) |> fst in
    Array.swap buttons i (length - 1);
    { buttons; length = length - 1 }
  ;;

  let rec fold' { buttons; length } acc i ~f =
    match i = length with
    | true -> acc
    | false -> fold' { buttons; length } (f acc buttons.(i)) (i + 1) ~f
  ;;

  let fold t ~init ~f = fold' t init 0 ~f

  let button_with_most_lights t =
    fold t ~init:(get_button t) ~f:(fun acc button ->
      match Button.num_counters acc >= Button.num_counters button with
      | true -> acc
      | false -> button)
  ;;
end

let minimum_presses ?(stop_if_greater_than = big_num) ~counter ~buttons () =
  let rec aux ~counter ~cur_pushes =
    let max_count = Counter.max_count counter in
    match max_count <= 0 || cur_pushes > stop_if_greater_than with
    | true -> cur_pushes
    | false ->
      let i = Counter.counter_index_with_max_count counter in
      let mega_button =
        Button_set.touching_counter_index buttons ~i
        |> Button_set.fold ~init:Button.empty ~f:Button.combine
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
  minimum_presses ~stop_if_greater_than:pushes ~counter ~buttons () < pushes
;;

let get_button_to_push ~counter ~buttons =
  let best_button, best_num_in_group =
    Counter.fold_counter_indices
      counter
      ~init:(Button_set.get_button buttons, big_num)
      ~f:(fun (best_button, num_buttons_in_group) i ->
        match num_buttons_in_group = 1 || Counter.count_at counter ~i = 0 with
        | true -> best_button, num_buttons_in_group
        | false ->
          let buttons = Button_set.touching_counter_index buttons ~i in
          let num_in_new_group = Button_set.length buttons in
          (match num_in_new_group > 0 with
           | false -> best_button, num_buttons_in_group
           | true ->
             (match
                [%compare: int] num_buttons_in_group num_in_new_group |> Ordering.of_int
              with
              | Less -> best_button, num_buttons_in_group
              | Equal ->
                let best_in_group = Button_set.button_with_most_lights buttons in
                (match
                   Button.num_counters best_in_group > Button.num_counters best_button
                 with
                 | true -> best_in_group, num_in_new_group
                 | false -> best_button, num_buttons_in_group)
              | Greater -> Button_set.button_with_most_lights buttons, num_in_new_group)))
  in
  let max_presses = Counter.max_times_can_press_button counter ~button:best_button in
  match best_num_in_group = 1 with
  | true -> best_button, max_presses, max_presses
  | false -> best_button, 0, max_presses
;;

(*= let best_button_sets =
    Counter.counter_indices counter
    |> List.fold ~init:[ buttons ] ~f:(fun acc i ->
      let buttons_in_group = Button_set.touching_counter_index buttons ~i in
      let num_buttons = Button_set.length buttons_in_group in
      match num_buttons > 0 with
      | false -> acc
      | true ->
        (match
           [%compare: int] num_buttons (Button_set.length (List.hd_exn acc))
           |> Ordering.of_int
         with
         | Less -> [ buttons_in_group ]
         | Greater -> acc
         | Equal -> buttons_in_group :: acc))
  in
  (* maybe choose the button that touches the most lights here? or the button with the
       minimum max presses? *)
  let a_button = List.hd_exn best_button_sets |> Button_set.get_button in
  match List.hd_exn best_button_sets |> Button_set.length = 1 with
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
;; *)

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
         List.range min_presses (max_presses + 1)
         |> List.fold ~init:best_presses ~f:(fun best_presses num_presses ->
           let counter = Counter.press_button counter ~button ~times:num_presses in
           let buttons = Button_set.remove buttons ~button in
           aux ~counter ~buttons ~cur_presses:(cur_presses + num_presses) ~best_presses))
  in
  aux ~counter ~buttons ~cur_presses:0 ~best_presses:big_num
;;

let parse_line s =
  let counter =
    strings_between s ~open_char:'{' ~close_char:'}' |> List.hd_exn |> Counter.parse
  in
  let buttons = Button_set.of_string s in
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

let part1 _ = failwith "unimplemented"
