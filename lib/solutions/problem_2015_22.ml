open! Core
open! Helpers

module State = struct
  type t = {
    mana : int;
    health : int;
    shield_time : int;
    poison_time : int;
    recharge_time : int;
    mana_spent : int;
    enemy_health : int;
    enemy_damage : int;
    turn : bool;
  }
  [@@deriving sexp]

  let magic_missile ({ mana; enemy_health; mana_spent; _ } as t) =
    if mana < 53 then None
    else
      Some
        {
          t with
          mana = mana - 53;
          enemy_health = Int.max 0 (enemy_health - 4);
          mana_spent = mana_spent + 53;
        }

  let drain ({ mana; health; enemy_health; mana_spent; _ } as t) =
    if mana < 73 then None
    else
      Some
        {
          t with
          mana = mana - 73;
          enemy_health = Int.max 0 (enemy_health - 2);
          health = health + 2;
          mana_spent = mana_spent + 73;
        }

  let shield ({ mana; shield_time; mana_spent; _ } as t) =
    if mana < 113 || shield_time > 0 then None
    else
      Some
        {
          t with
          mana = mana - 113;
          mana_spent = mana_spent + 113;
          shield_time = 6;
        }

  let poison ({ mana; poison_time; mana_spent; _ } as t) =
    if mana < 173 || poison_time > 0 then None
    else
      Some
        {
          t with
          mana = mana - 173;
          mana_spent = mana_spent + 173;
          poison_time = 6;
        }

  let recharge ({ mana; recharge_time; mana_spent; _ } as t) =
    if mana < 229 || recharge_time > 0 then None
    else
      Some
        {
          t with
          mana = mana - 229;
          mana_spent = mana_spent + 229;
          recharge_time = 5;
        }

  let enemy_turn
      ({
         mana;
         health;
         shield_time;
         poison_time;
         recharge_time;
         enemy_health;
         enemy_damage;
         _;
       } as t) =
    let enemy_health, poison_time =
      if poison_time > 0 then (enemy_health - 3, poison_time - 1)
      else (enemy_health, 0)
    in
    let mana, recharge_time =
      if recharge_time > 0 then (mana + 101, recharge_time - 1) else (mana, 0)
    in
    let health, shield_time =
      if enemy_health <= 0 then (health, Int.max 0 (shield_time - 1))
      else if shield_time > 0 then
        let dam = Int.max 1 (enemy_damage - 7) in
        (health - dam, shield_time - 1)
      else (health - enemy_damage, shield_time)
    in
    {
      t with
      mana;
      health;
      shield_time;
      poison_time;
      recharge_time;
      enemy_health;
      turn = true;
    }

  let my_turn
      ({ mana; shield_time; poison_time; recharge_time; enemy_health; _ } as t)
      move =
    let enemy_health, poison_time =
      if poison_time > 0 then (enemy_health - 3, poison_time - 1)
      else (enemy_health, 0)
    in
    let mana, recharge_time =
      if recharge_time > 0 then (mana + 101, recharge_time - 1) else (mana, 0)
    in
    let shield_time = if shield_time > 0 then shield_time - 1 else 0 in
    let t =
      { t with mana; shield_time; poison_time; recharge_time; enemy_health }
    in
    match move t with Some t -> Some { t with turn = false } | None -> None

  let get_next_things t =
    match t.turn with
    | false -> [ enemy_turn t ]
    | true ->
        List.filter_map [ magic_missile; drain; shield; poison; recharge ]
          ~f:(fun move -> my_turn t move)

  let get_next_things_hard t =
    match t.turn with
    | false -> [ enemy_turn t ]
    | true ->
        if t.health = 1 then []
        else
          let t = { t with health = t.health - 1 } in
          List.filter_map [ magic_missile; drain; shield; poison; recharge ]
            ~f:(fun move -> my_turn t move)

  let compare { mana_spent = n1; _ } { mana_spent = n2; _ } = Int.compare n1 n2

  let of_string s =
    let enemy_health, s = Parse.take_next_int s in
    let enemy_damage, _ = Parse.take_next_int s in
    {
      mana = 500;
      health = 50;
      shield_time = 0;
      poison_time = 0;
      recharge_time = 0;
      mana_spent = 0;
      enemy_health;
      enemy_damage;
      turn = true;
    }
end

module Pq = Priority_queue.Make (State)

let djikstra ?(neighbor_states = State.get_next_things) start_state =
  let rec aux pq =
    let cur, pq = Pq.get_exn pq in
    if cur.enemy_health <= 0 then cur.mana_spent
    else
      let neighbors =
        neighbor_states cur |> List.filter ~f:(fun { health; _ } -> health > 0)
      in
      aux (List.fold neighbors ~init:pq ~f:(fun pq state -> Pq.add pq state))
  in
  aux (Pq.singleton start_state)

let part1 s =
  let start_state = State.of_string s in
  djikstra start_state |> Int.to_string |> Ok

let part2 s =
  let start_state = State.of_string s in
  djikstra ~neighbor_states:State.get_next_things_hard start_state
  |> Int.to_string |> Ok
