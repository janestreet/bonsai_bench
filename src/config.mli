open! Core
open! Bonsai
module Interaction = Bonsai_bench_scenario.Interaction

module Interactions : sig
  type ('a, 'r) t =
    { time_source : Time_source.t
    ; name : string
    ; component : local_ graph -> 'r Bonsai.t
    ; get_inject : 'r -> 'a -> unit Effect.t
    ; interaction : 'a Interaction.t
    }
end

module Startup : sig
  type 'a t =
    { time_source : Bonsai.Time_source.t
    ; name : string
    ; component : local_ graph -> 'a Bonsai.t
    }
end

type t =
  | Interactions : (_, _) Interactions.t -> t
  | Startup : _ Startup.t -> t

val create
  :  ?time_source:Bonsai.Time_source.t
  -> name:string
  -> component:(local_ graph -> 'r Bonsai.t)
  -> get_inject:('r -> 'a -> unit Effect.t)
  -> 'a Interaction.t
  -> t

val create_with_resetter
  :  ?time_source:Bonsai.Time_source.t
  -> name:string
  -> component:(local_ graph -> 'r Bonsai.t)
  -> get_inject:('r -> 'a -> unit Effect.t)
  -> 'a Interaction.t
  -> t

val create_for_startup
  :  ?time_source:Bonsai.Time_source.t
  -> name:string
  -> (local_ graph -> 'r Bonsai.t)
  -> t

val startup_get_inject : _ -> _ -> unit Effect.t
