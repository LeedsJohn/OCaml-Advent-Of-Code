open! Core
include Directed_graph_intf

module Extend : EXTENDER =
functor
  (Arg : S)
  ->
  struct
    open! Arg

    let bfs ?(verbose = false) start_pos ~is_goal =
      let max_steps = ref 0 in
      let visited = Hash_set.create (module Arg) in
      Hash_set.add visited start_pos;
      let q = Queue.create () in
      let rec aux () =
        let cur, steps = Queue.dequeue_exn q in
        if verbose && steps > !max_steps then (
          print_s
            [%message
              ""
                ~queue_length:(Queue.length q : int)
                (steps : int)
                ~visited_states:(Hash_set.length visited : int)];
          max_steps := steps);
        if is_goal cur then steps
        else (
          get_neighbor_states cur
          |> List.iter ~f:(fun state ->
                 if Hash_set.mem visited state then ()
                 else (
                   Hash_set.add visited state;
                   Queue.enqueue q (state, steps + 1)));
          aux ())
      in
      Queue.enqueue q (start_pos, 0);
      aux ()

    module Pq = Priority_queue.Make (Arg)

    let djikstra start_pos ~is_goal =
      let visited = Hash_set.create (module Arg) in
      let rec aux pq =
        let cur, pq = Pq.get_exn pq in
        Hash_set.add visited cur;
        if is_goal cur then cur
        else
          get_neighbor_states cur
          |> List.filter ~f:(fun state -> not (Hash_set.mem visited state))
          |> List.fold ~init:pq ~f:(fun pq state -> Pq.add pq state)
          |> aux
      in
      aux (Pq.singleton start_pos)
  end
