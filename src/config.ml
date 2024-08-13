open! Core
open! Bonsai
module Interaction = Bonsai_perf_shared.Interaction

module Interactions = struct
  type ('a, 'r) t =
    { time_source : Bonsai.Time_source.t
    ; name : string
    ; component : Bonsai.graph -> 'r Bonsai.t
    ; get_inject : 'r -> 'a -> unit Effect.t
    ; interaction : 'a Interaction.t
    }
end

module Startup = struct
  type 'a t =
    { time_source : Bonsai.Time_source.t
    ; name : string
    ; component : Bonsai.graph -> 'a Bonsai.t
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
        [ interaction; Interaction.reset_model; Interaction.stabilize ]
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
