open! Core
open! Async

module Run_type = struct
  type t =
    | Test
    | Real
    | Submit

  let arg =
    Command.Arg_type.create (fun s ->
      match s with
      | "test" -> Test
      | "real" -> Real
      | "submit" -> Submit
      | _ -> failwith "Invalid run type")
  ;;
end

type t =
  { run_type : Run_type.t
  ; part : int
  }
