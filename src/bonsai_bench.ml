open! Core
include Config
module Input = Bonsai_perf_shared.Input
module Scenario = Bonsai_perf_shared.Scenario

module type Config = Bonsai_perf_shared.Config

module Cleanup : sig
  val schedule : Runner.t -> unit
  val cleanup : unit -> unit
end = struct
  let scheduled = ref []
  let schedule runner = scheduled := runner :: !scheduled

  let cleanup () =
    let to_cleanup = !scheduled in
    scheduled := [];
    List.iter ~f:Runner.invalidate_observers to_cleanup;
    Gc.full_major ()
  ;;
end

let to_core_bench_test = function
  | Interactions { time_source; name; component; get_inject; interaction } ->
    let bonsai_bench_initialize_run `init =
      Cleanup.cleanup ();
      let runner =
        Runner.initialize
          ~time_source
          ~component
          ~wrap_driver_creation:{ f = (fun create_driver -> create_driver ()) }
          ~get_inject
          ~interaction
          ~filter_profiles:true
      in
      Cleanup.schedule runner;
      fun () -> Runner.run_interactions runner ~handle_profile:(Fn.const ())
    in
    Core_bench_js.Test.create_with_initialization ~name bonsai_bench_initialize_run
  | Startup { time_source; name; component } ->
    let gc_before_run `init =
      Cleanup.cleanup ();
      fun () ->
        let runner =
          Runner.initialize
            ~time_source
            ~component
            ~wrap_driver_creation:{ f = (fun create_driver -> create_driver ()) }
            ~get_inject:(fun _ _ -> Bonsai.Effect.Ignore)
            ~interaction:Interaction.stabilize
            ~filter_profiles:true
        in
        Runner.run_interactions runner ~handle_profile:(Fn.const ());
        Cleanup.schedule runner
    in
    Core_bench_js.Test.create_with_initialization ~name gc_before_run
;;

let benchmark ?run_config ?analysis_configs ?display_config ?save_to_file ?libname ts =
  ts
  |> List.map ~f:to_core_bench_test
  |> Core_bench_js.bench
       ?run_config
       ?analysis_configs
       ?display_config
       ?save_to_file
       ?libname
;;

let measure ?run_config ts =
  ts |> List.map ~f:to_core_bench_test |> Core_bench_js.measure ?run_config
;;

let profile profiles = List.iter ~f:Profile.profile profiles

let benchmark_compare
  (type conf)
  ?print_separate_rows
  ?run_config
  (module Config : Config with type t = conf)
  ~(tests : (conf -> t) list)
  configs
  =
  let module Config' = struct
    include Config
    include Comparator.Make (Config)
  end
  in
  let module Scenario = struct
    type t = string * int [@@deriving sexp]

    let compare (_, a) (_, b) = Int.compare a b
  end
  in
  let module Scenario_map = Map.Make (Scenario) in
  List.map configs ~f:(fun config ->
    let results =
      measure ?run_config (List.map tests ~f:(fun t -> t config))
      |> List.map ~f:Core_bench_js.analyze
      |> List.filter_map ~f:(function
        | Error err ->
          eprintf "Error %s" (Error.to_string_hum err);
          None
        | Ok r -> Some r)
      |> Core_bench_internals.Simplified_benchmark.extract
      |> List.mapi ~f:(fun i { full_benchmark_name; time_per_run_nanos; _ } ->
        (full_benchmark_name, i), Float.to_string_hum ~decimals:2 time_per_run_nanos)
      |> Scenario_map.of_alist_exn
    in
    config, results)
  |> Map.of_alist_exn (module Config')
  |> Map.transpose_keys (module Scenario_map.Key)
  |> Map.map ~f:(fun conf_map ->
    Map.to_alist conf_map
    |> List.map
         ~f:
           (Tuple2.map_fst ~f:(fun c -> (Config.name c |> String.uncapitalize) ^ " (ns)")))
  |> Map.to_alist
  |> List.map ~f:(fun ((scenario, _), result) -> scenario, result)
  |> Expectable.print_alist
       ?separate_rows:print_separate_rows
       [%sexp_of: (string * string) list]
;;

let benchmark_compare_interactions
  (type conf input action)
  ?print_separate_rows
  ?run_config
  (module Config : Config
    with type t = conf
     and type input = input
     and type action = action)
  ~scenarios
  ~configs
  =
  let tests =
    List.map scenarios ~f:(fun { Scenario.initial; test_name; interaction } config ->
      let input = Input.create initial in
      create
        ~name:test_name
        ~component:(Config.computation config (Input.value input))
        ~get_inject:Config.get_inject
        (interaction input))
  in
  benchmark_compare ?print_separate_rows ?run_config (module Config) ~tests configs
;;

let const_value_not_constant_folded x = Input.(value (create x))

let benchmark_compare_startup
  (type conf input action)
  ?print_separate_rows
  ?run_config
  (module Config : Config
    with type t = conf
     and type input = input
     and type action = action)
  ~inputs
  ~configs
  =
  let tests =
    List.map inputs ~f:(fun (name, input) config ->
      create_for_startup
        ~name
        (Config.computation config (const_value_not_constant_folded input)))
  in
  benchmark_compare ?print_separate_rows ?run_config (module Config) ~tests configs
;;
