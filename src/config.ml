open! Core
open! Bonsai
module Interaction = Bonsai_bench_scenario.Interaction

module Interactions = struct
  type ('a, 'r) t =
    { time_source : Bonsai.Time_source.t
    ; name : string
    ; component : local_ Bonsai.graph -> 'r Bonsai.t
    ; get_inject : 'r -> 'a -> unit Effect.t
    ; interaction : 'a Interaction.t
    }
end

module Startup = struct
  type 'r t =
    { time_source : Bonsai.Time_source.t
    ; name : string
    ; component : local_ Bonsai.graph -> 'r Bonsai.t
    }
end

type t =
  | Interactions : (_, _) Interactions.t -> t
  | Startup : _ Startup.t -> t

let create
  ?(time_source = Bonsai.Time_source.create ~start:Time_ns.epoch)
  ~name
  ~component
  ~get_inject
  interaction
  =
  Interactions { time_source; name; component; get_inject; interaction }
;;

let create_with_resetter
  ?(time_source = Bonsai.Time_source.create ~start:Time_ns.epoch)
  ~name
  ~component
  ~get_inject
  interaction
  =
  Interactions
    { time_source
    ; name
    ; component
    ; get_inject
    ; interaction =
        [ interaction; Interaction.reset_model; Interaction.recompute ]
        |> Interaction.many
    }
;;

let create_for_startup
  ?(time_source = Bonsai.Time_source.create ~start:Time_ns.epoch)
  ~name
  component
  =
  Startup { time_source; name; component }
;;

let startup_get_inject _ _ =
  Bonsai.Effect.of_thunk (fun () ->
    raise_s [%message "Benchmarking startup does not support [inject] interactions."])
;;
