open! Core
open Bonsai.For_open
open Bonsai_bench_scenario
module Interaction = Bonsai_bench_scenario.Interaction
module Input = Bonsai_bench_scenario.Input
module Scenario = Bonsai_bench_scenario.Scenario

(** [t] is roughly equivalent to [Core_bench_js.Test.t], but can also be used to obtain
    [profile]s of the benchmarks. See [profile] below for more details. *)
type t

(** {2 Defining Benchmarks} *)

(** [create] produces a benchmark which performs [interactions] on [component]. The
    computation is shared between runs within the benchmark runner. Since they are run a
    non-deterministic amount of times, benchmarks created this way should either have an
    interaction which is idempotent on the state, or have similar performance when the
    interaction is repeated many times. *)
val create
  :  ?time_source:Bonsai.Time_source.t
  -> name:string
  -> component:(local_ Bonsai.graph -> 'r Bonsai.t)
  -> get_inject:('r -> 'a -> unit Effect.t)
  -> 'a Interaction.t
  -> t

(** [create_with_resetter] is equivalent to calling [create], with interactions equal to
    [Interaction.many interactions; Interaction.reset_model; Interaction.recompute]. *)
val create_with_resetter
  :  ?time_source:Bonsai.Time_source.t
  -> name:string
  -> component:(local_ Bonsai.graph -> 'r Bonsai.t)
  -> get_inject:('r -> 'a -> unit Effect.t)
  -> 'a Interaction.t
  -> t

(** [create_for_startup] produces a benchmark for the Bonsai instantiation and first
    stabilization of a component. *)
val create_for_startup
  :  ?time_source:Bonsai.Time_source.t
  -> name:string
  -> (local_ Bonsai.graph -> 'a Bonsai.t)
  -> t

module Benchmark_set : sig
  (** A [Benchmark_set.t] is a named collection of [t]s. Benchmark sets also enable
      printing comparison benchmarks as a table, if ran directly as a set. *)
  type t
end

(** [set] packages some [t]s into a [Benchmark_set.t] *)
val set : name:string -> t list -> Benchmark_set.t

(** [profile] runs some [t]s as instrumented computation, and provides snapshots of how
    much time is spent within different parts of bonsai code. It also provides statistics
    on incremental overhead.

    Each snapshot only includes timing **since the last snapshot**; i.e. snapshots are not
    cumulative.

    By default, [profile] will take a snapshot after startup, and another after running
    all interactions. You can add additional snapshots via [Interaction.profile].

    Note: because [profile] runs on an instrumented computation, the total running time of
    the test may be higher. Furthermore, because [profile] only runs the computation once,
    timing may vary between runs. It is useful for drilling into slow benchmarks, but
    [benchmark] should be the source of truth for timing interactions.

    NOTE: profile will not currently contribute any machine output. *)
val profile : name:string -> t list -> Benchmark_set.t

(** [compare_startup] measures the startup time of various configurations of a
    computation, for a variety of inputs. *)
val compare_startup
  :  ?print_separate_rows:bool
  -> name:string
  -> computations:(string * ('input, 'output) compare_computation) list
  -> (string * 'input) list
  -> Benchmark_set.t

(** [compare_interactions] allows you to run some [Bonsai_driver.Scenario.t]s across
    multiple "configurations" of a computation, and will print a separate column for each
    configuration's output. *)
val compare_interactions
  :  ?print_separate_rows:bool
  -> name:string
  -> get_inject:('output -> ('action -> unit Bonsai.Effect.t))
  -> computations:(string * ('input, 'output) compare_computation) list
  -> ('input, 'action) Scenario.t list
  -> Benchmark_set.t

(** {2 Running Benchmarks} *)

(** [run_via_command] runs some [t]s, using [Command_nodejs] to parse CLI args such as
    [-quota]. Only one instance of [run_via_command] or [run_sets_via_command] may be used
    per executable.

    If you pass [-machine-output-file=<FILE_PATH>] via a CLI arg, a serialized
    [Bonsai_bench_protocol.Machine_output.t] will be written to <FILE_PATH>. Any analysis
    / display-specific configs will be used for the results printed to stdout, but not for
    the machine output. *)
val run_via_command : here:[%call_pos] -> t list -> unit

(** [run_sets_via_command] runs some [Benchmark_set.t]s, using [Command_nodejs] to parse
    CLI args such as [-quota]. Only one instance of [run_via_command] or
    [run_sets_via_command] may be used per executable.

    If you pass [-machine-output-file=<FILE_PATH>] via a CLI arg, a serialized
    [Bonsai_bench_protocol.Machine_output.t] will be written to <FILE_PATH>. Any analysis
    / display-specific configs will be used for the results printed to stdout, but not for
    the machine output. *)
val run_sets_via_command : here:[%call_pos] -> Benchmark_set.t list -> unit

(** [For_testing] contains utils for running [t]s directly, as a way to validate that the
    benchmark is doing what's expected. *)
module For_testing : sig
  (** [benchmark] works similarly to [Core_bench_js.bench], but ensures that [Observer]s
      involved in benchmarks are cleaned up between consecutive benchmarks. *)
  val benchmark
    :  ?save_measurement:(Core_bench_js.Measurement.t -> unit)
    -> ?run_config:Core_bench_js.Run_config.t
    -> ?analysis_configs:Core_bench_js.Analysis_config.t list
    -> ?display_config:Core_bench_js.Display_config.t
    -> ?libname:string
    -> t list
    -> unit

  (** [measure] works identically to [Core_bench_js.measure], but ensures that [Observer]s
      involved in benchmarks are cleaned up between consecutive benchmarks. *)
  val measure
    :  ?run_config:Core_bench_js.Run_config.t
    -> t list
    -> Core_bench_js.Measurement.t list

  (** Like [profile] above, but ran directly. *)
  val profile : t list -> unit
end
