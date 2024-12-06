open! Core

(* module Amphipod = struct
       type t = No | A | B | C | D [@@deriving equal]

     let to_int = function No -> 0 | A -> 1 | B -> 2 | C -> 3 | D -> 4
     let of_int = function 1 -> A | 2 -> B | 3 -> C | 4 -> D | _ -> No

     let energy_cost = function
         | A -> 1
         | B -> 10
         | C -> 100
         | D -> 1000
         | No -> 0

       let goal_room amph = (to_int amph) - 1
   end

   module Pos = struct
     module T = struct
       module U = struct
         type t = Hallway of int | Room of int * bool [@@deriving compare, equal, hash, sexp]
       end

       include U
       include Comparator.Make (U)
     end

     include T

     let to_int = function
       | Hallway n -> n
       | Room (room_num, deep) -> 11 + (2 * room_num) + Bool.to_int deep

     let of_int n =
       if Int.between n ~low:0 ~high:10 then Hallway n
       else Room ((n - 11) / 2, not (n % 2 = 1))

     let all = List.map (List.range 0 19) ~f:of_int

     let neighbors pos =
       let hallway_neighbors =
         match pos with
         | Hallway 0 -> [ Hallway 1 ]
         | Hallway 10 -> [ Hallway 9 ]
         | Hallway n -> [ Hallway (n - 1); Hallway (n + 1) ]
         | Room (n, false) -> [ Hallway ((n * 2) + 2) ]
         | _ -> []
       in
       let room_neighbors =
         match pos with
         | Hallway 2 -> [ Room (0, false) ]
         | Hallway 4 -> [ Room (1, false) ]
         | Hallway 6 -> [ Room (2, false) ]
         | Hallway 8 -> [ Room (3, false) ]
         | Room (n, deep) -> [ Room (n, not deep) ]
         | _ -> []
       in
       Set.of_list (module T) (hallway_neighbors @ room_neighbors)
   end

   module Burrow = struct
     type t = int

     let get t pos = t / Int.pow 5 (Pos.to_int pos) % 5 |> Amphipod.of_int

     let set t pos amph =
       let amph = Amphipod.to_int amph in
       let cur_val = get t pos |> Amphipod.to_int in
       let dif = cur_val - amph in
       t + (Int.pow 5 (Pos.to_int pos) * dif)

     let steps_to_room t pos =
         let amph = get t pos in
         let visited = Hash_set.create (module Pos) in
         Hash_set.add visited pos;
         let goal_spot = Pos.Room (Amphipod.goal_room amph, false) in
         let rec aux level steps =
             if List.exists ~f:(Pos.equal goal_spot) then (
                 match get t (Pos.Room (Amphipod.goal_room amph, true)) with
                 | Amphipod.No -> steps + 1, true
                 | a -> if Amphipod.equal a amph then steps, false else -1, false
             )
             else
                 let next_spots =
                     List.map level ~f:Pos.neighbors |> Set.union_list (module Pos)
                 in
                 List.find level ~f:(fun pos ->

                     if Hash_set.mem visited pos then false else (

                     )
                 )





   end *)

let part1 _ = Error (Error.of_string "Unimplemented")
let part2 _ = Error (Error.of_string "Unimplemented")
