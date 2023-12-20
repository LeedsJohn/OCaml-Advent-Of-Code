open Core

let day = 20
let year = 2023

type pulse = Low | High [@@deriving compare, equal, sexp]

type flip = { name : string; on : bool; outputs : string list }
[@@deriving compare, sexp]

module String_map = Map.Make (String)

type conjunction = {
  name : string;
  last_pulses : pulse String_map.t;
  outputs : string list;
}
[@@deriving compare, sexp]

type broadcaster = { name : string; outputs : string list }
[@@deriving compare, sexp]

let all_high_pulses conj =
  Map.for_all conj.last_pulses ~f:(fun p -> equal_pulse p High)

type thing =
  | Broadcaster of broadcaster
  | Flip of flip
  | Conjunction of conjunction
[@@deriving compare, sexp]

let parse_text text =
  let text = String.strip text |> String.split_lines in
  let get_outputs line =
    let outputs = String.split line ~on:'>' in
    List.nth_exn outputs 1 |> String.split ~on:',' |> List.map ~f:String.strip
  in
  let get_type line =
    match String.get line 0 with
    | '%' -> `Flip
    | '&' -> `Conjunction
    | _ -> `Broadcaster
  in
  let get_name line =
    match get_type line with
    | `Broadcaster -> "broadcaster"
    | `Flip | `Conjunction -> String.slice line 1 (String.index_exn line ' ')
  in
  let get_inputs name =
    (* inefficient but only run once so it should be OK *)
    List.fold text ~init:String_map.empty ~f:(fun acc line ->
        if List.mem (get_outputs line) name ~equal:String.equal then
          Map.add_exn acc ~key:(get_name line) ~data:Low
        else acc)
  in
  List.fold text ~init:String_map.empty ~f:(fun acc line ->
      let outputs = get_outputs line in
      let name = get_name line in
      let data =
        match get_type line with
        | `Broadcaster -> Broadcaster { name; outputs }
        | `Flip -> Flip { name; on = false; outputs }
        | `Conjunction ->
            Conjunction { name; last_pulses = get_inputs name; outputs }
      in
      Map.add_exn acc ~key:name ~data)

let get_next_pulses things input_name pulse thing =
  match thing with
  | Broadcaster thing ->
      ( things,
        List.map thing.outputs ~f:(fun output -> (thing.name, pulse, output)) )
  | Flip thing -> (
      match pulse with
      | High -> (things, [])
      | Low ->
          let new_thing = { thing with on = not thing.on } in
          let output_pulse = if thing.on then Low else High in
          let next_pulses =
            List.map thing.outputs ~f:(fun output ->
                (thing.name, output_pulse, output))
          in
          ( Map.update things thing.name ~f:(fun _ -> Flip new_thing),
            next_pulses ))
  | Conjunction thing ->
      let new_memory =
        Map.update thing.last_pulses input_name ~f:(fun _ -> pulse)
      in
      let new_thing = { thing with last_pulses = new_memory } in
      let output_pulse = if all_high_pulses new_thing then Low else High in
      let next_pulses =
        List.map thing.outputs ~f:(fun output ->
            (thing.name, output_pulse, output))
      in
      ( Map.update things thing.name ~f:(fun _ -> Conjunction new_thing),
        next_pulses )

let push_button things find_name =
  let rec aux things cur_pulses num_low num_high output_high :
      thing String_map.t * int * int * bool =
    if List.is_empty cur_pulses then (things, num_low, num_high, output_high)
    else
      let new_things, next_pulses, num_low, num_high, output_high =
        List.fold cur_pulses ~init:(things, [], num_low, num_high, output_high)
          ~f:(fun
              (things, next_pulses, num_low, num_high, output_high)
              (input_name, pulse, name)
            ->
            let num_low, num_high =
              match pulse with
              | Low -> (num_low + 1, num_high)
              | High -> (num_low, num_high + 1)
            in
            let new_things, additional_pulses =
              match Map.find things name with
              | None -> (things, [])
              | Some thing -> get_next_pulses things input_name pulse thing
            in
            let output_high =
              output_high
              || String.(input_name = find_name && name = "zr")
                 && equal_pulse High pulse
            in
            ( new_things,
              next_pulses @ additional_pulses,
              num_low,
              num_high,
              output_high ))
      in
      aux new_things next_pulses num_low num_high output_high
  in
  aux things [ ("button", Low, "broadcaster") ] 0 0 false

let part1 fname =
  let things = parse_text (In_channel.read_all fname) in
  let _, num_low, num_high =
    List.fold (List.range 0 1000) ~init:(things, 0, 0)
      ~f:(fun (things, num_low, num_high) _ ->
        let new_things, additional_low, additional_high, _ =
          push_button things ""
        in
        (new_things, num_low + additional_low, num_high + additional_high))
  in
  num_low * num_high |> Int.to_string

let num_until_output_high things name =
  let i = ref 0 in
  let res = ref None in
  let things = ref things in
  while Option.is_none !res do
    let new_things, _, _, thing_output_high = push_button !things name in
    if thing_output_high then res := Some !i;
    things := new_things;
    i := !i + 1
  done;
  Option.value_exn !res

let part2 fname =
  (* this problem sucks *)
  let things = parse_text (In_channel.read_all fname) in
  let connected = [ "cm"; "gc"; "sz"; "xf" ] in
  List.map connected ~f:(fun name -> num_until_output_high things name)
  |> List.fold ~init:1 ~f:(fun acc n -> acc * (n + 1))
  |> Int.to_string
