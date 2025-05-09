open Bonsai_proc.For_open
module Interaction = Bonsai_bench_scenario.Interaction

type t
type wrap_create = { f : 'a. (unit -> 'a) -> 'a } [@@unboxed]

val initialize
  :  filter_profiles:bool
  -> driver_instrumentation:
       ( Bonsai_driver.Instrumentation.Timeable_event.t
         , _ )
         Bonsai.Private.Instrumentation.Config.t
  -> wrap_driver_creation:wrap_create
  -> time_source:Bonsai.Time_source.t
  -> component:'r Computation.t
  -> get_inject:('r -> 'a -> unit Effect.t)
  -> interaction:'a Interaction.t
  -> t

val run_interactions : t -> handle_profile:(string -> unit) -> unit
val invalidate_observers : t -> unit
