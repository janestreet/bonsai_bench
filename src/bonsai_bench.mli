open! Core
open Bonsai.For_open
module Interaction = Bonsai_perf_shared.Interaction
module Input = Bonsai_perf_shared.Input
module Scenario = Bonsai_perf_shared.Scenario

module type Config = Bonsai_perf_shared.Config

(** [t] is roughly equivalent to [Core_bench_js.Test.t], but can also be used to obtain
    [profile]s of the benchmarks. See [profile] below for more details. *)
type t

(** [create] produces a benchmark which performs [interactions] on [component].
    The computation is shared between runs within the benchmark runner. Since they are
    run a non-deterministic amount of times, benchmarks created this way should either
    have an interaction which is idempotent on the state, or have similar performance
    when the interaction is repeated many times. *)
val create
  :  ?time_source:Bonsai.Time_source.t
  -> name:string
  -> component:(Bonsai.graph -> 'r Bonsai.t)
  -> get_inject:('r -> 'a -> unit Effect.t)
  -> 'a Interaction.t
  -> t

(** [create_with_resetter] is equivalent to calling [create], with interactions equal to
    [Interaction.many interactions; Interaction.reset_model; Interaction.stabilize]. *)
val create_with_resetter
  :  ?time_source:Bonsai.Time_source.t
  -> name:string
  -> component:(Bonsai.graph -> 'r Bonsai.t)
  -> get_inject:('r -> 'a -> unit Effect.t)
  -> 'a Interaction.t
  -> t

(** [create_for_startup] produces a benchmark for the Bonsai instantiation and first
    stabilization of a component. *)
val create_for_startup
  :  ?time_source:Bonsai.Time_source.t
  -> name:string
  -> (Bonsai.graph -> 'a Bonsai.t)
  -> t

(** [benchmark] works identically to [Core_bench_js.bench], but ensures that [Observer]s
    involved in benchmarks are cleaned up between consecutive benchmarks. *)
val benchmark
  :  ?run_config:Core_bench_js.Run_config.t
  -> ?analysis_configs:Core_bench_js.Analysis_config.t list
  -> ?display_config:Core_bench_js.Display_config.t
  -> ?save_to_file:(Core_bench_js.Measurement.t -> string)
  -> ?libname:string
  -> t list
  -> unit

(** [measure] works identically to [Core_bench_js.measure], but ensures that [Observer]s
    involved in benchmarks are cleaned up between consecutive benchmarks. *)
val measure
  :  ?run_config:Core_bench_js.Run_config.t
  -> t list
  -> Core_bench_js.Measurement.t list

(** [profile] runs a given [t] as an instrumented computation, and provides snapshots of
    how much time is spent within different parts of bonsai code. It also provides
    statistics on incremental overhead.

    Note: because [profile] runs on an instrumented computation, the total running time
    of the test may be higher. Furthermore, because [profile] only runs the computation
    once, timing may vary between runs. It is useful for drilling into slow benchmarks,
    but [benchmark] should be the source of truth for timing interactions. *)
val profile : t list -> unit

(** [benchmark_compare_interactions] allows you to run some [Bonsai_driver.Scenario.t]s
    across multiple "configurations" of a computation, and will print a separate column
    for each configuration's output.
*)
val benchmark_compare_interactions
  :  ?print_separate_rows:bool
  -> ?run_config:Core_bench_internals.Run_config.t
  -> (module Config with type t = 'conf and type input = 'input and type action = 'action)
  -> scenarios:('input, 'action) Scenario.t list
  -> configs:'conf list
  -> unit

(** [benchmark_compare_startup] measures the startup time of various configurations
    of a computation, for a variety of inputs. *)
val benchmark_compare_startup
  :  ?print_separate_rows:bool
  -> ?run_config:Core_bench_internals.Run_config.t
  -> (module Config with type t = 'conf and type input = 'input and type action = 'action)
  -> inputs:(string * 'input) list
  -> configs:'conf list
  -> unit
