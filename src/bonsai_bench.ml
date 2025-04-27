open! Core
include Config
module Input = Bonsai_perf_shared.Input
module Scenario = Bonsai_perf_shared.Scenario

module Machine_output = struct
  let mode =
    let output_mode_envvar = Bonsai_bench_protocol.Private.machine_output_mode_envvar in
    let output_tempdir_envvar =
      Bonsai_bench_protocol.Private.machine_output_tempdir_envvar
    in
    lazy
      (let output_mode =
         Sys.getenv output_mode_envvar |> Option.map ~f:String.lowercase
       in
       let output_tempdir = Sys.getenv output_tempdir_envvar in
       match output_mode, output_tempdir with
       | None, None -> None
       | Some output_mode, None ->
         raise_s
           [%message
             "Bonsai bench machine output enabled, but no tempdir specified."
               (output_mode_envvar : string)
               (output_mode : string)
               (output_tempdir_envvar : string)]
       | None, Some output_tempdir ->
         raise_s
           [%message
             "Bonsai bench machine output tempdir specified, but no mode declared."
               (output_mode_envvar : string)
               (output_tempdir : string)
               (output_tempdir_envvar : string)]
       | Some ("test" | "testing"), Some tempdir -> Some (`Test tempdir)
       | Some ("prod" | "production"), Some tempdir -> Some (`Prod tempdir)
       | Some invalid_value, _ ->
         raise_s
           [%message
             "Invalid value for env var"
               (output_mode_envvar : string)
               (invalid_value : string)])
  ;;

  let save_to_file ~comparison_config_name =
    let%map.Option (`Prod tempdir | `Test tempdir) = force mode in
    fun measurement ->
      let filename =
        Bonsai_bench_protocol.Dimensions.(
          to_filename
            (V1
               { benchmark_name = Core_bench_internals.Measurement.name measurement
               ; comparison_config_name
               ; backend =
                   (match Sys.backend_type with
                    | Native -> "native"
                    | Bytecode -> "bytecode"
                    | Other other -> other)
               ; tags = String.Map.empty
               }))
      in
      Filename.concat tempdir filename
  ;;

  let run_config run_config =
    let default =
      match run_config with
      | None -> Core_bench_js.Run_config.create ()
      | Some run_config -> run_config
    in
    match force mode with
    | None -> default
    | Some (`Prod _) -> { default with stabilize_gc_between_runs = true }
    | Some (`Test _) ->
      { default with quota = Core_bench_js.Quota.Span (Time_float.Span.of_ms 10.) }
  ;;
end

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
          ~driver_instrumentation:
            (Bonsai_driver.Instrumentation.default_for_test_handles ())
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
            ~driver_instrumentation:
              (Bonsai_driver.Instrumentation.default_for_test_handles ())
            ~wrap_driver_creation:{ f = (fun create_driver -> create_driver ()) }
            ~get_inject:(fun _ _ -> Bonsai.Effect.Ignore)
            ~interaction:Interaction.recompute
            ~filter_profiles:true
        in
        Runner.run_interactions runner ~handle_profile:(Fn.const ());
        Cleanup.schedule runner
    in
    Core_bench_js.Test.create_with_initialization ~name gc_before_run
;;

let benchmark ?run_config ?analysis_configs ?display_config ?libname ts =
  ts
  |> List.map ~f:to_core_bench_test
  |> Core_bench_js.bench
       ~run_config:(Machine_output.run_config run_config)
       ?analysis_configs
       ?display_config
       ?save_to_file:(Machine_output.save_to_file ~comparison_config_name:None)
       ?libname
;;

let measure ?run_config ts =
  ts
  |> List.map ~f:to_core_bench_test
  |> Core_bench_js.measure ~run_config:(Machine_output.run_config run_config)
;;

let profile profiles =
  if Option.is_some (force Machine_output.mode)
  then
    print_endline
      [%string
        "Bonsai_bench.profile cannot currently be serialized to a machine-ingestible \
         format for automated benchmark runners."];
  List.iter ~f:Profile.profile profiles
;;

let benchmark_compare ?print_separate_rows ?run_config ~computations ~tests () =
  let module Ordered_strings = struct
    type t = string * int [@@deriving sexp]

    let compare (_, a) (_, b) = Int.compare a b
  end
  in
  let module Ordered_strings_map = Map.Make (Ordered_strings) in
  List.mapi computations ~f:(fun i (comp_name, computation) ->
    let maybe_save_to_file =
      match Machine_output.save_to_file ~comparison_config_name:(Some comp_name) with
      | None -> Fn.id
      | Some to_filename ->
        fun measurement ->
          Core_bench_js.Measurement.save measurement ~filename:(to_filename measurement);
          measurement
    in
    let results =
      measure
        ~run_config:(Machine_output.run_config run_config)
        (List.map tests ~f:(fun t -> t computation))
      |> List.map ~f:maybe_save_to_file
      |> List.map ~f:Core_bench_js.analyze
      |> List.filter_map ~f:(function
        | Error err ->
          eprintf "Error %s" (Error.to_string_hum err);
          None
        | Ok r -> Some r)
      |> Core_bench_internals.Simplified_benchmark.extract
      |> List.mapi ~f:(fun i { full_benchmark_name; time_per_run_nanos; _ } ->
        (full_benchmark_name, i), Time_ns.Span.of_ns time_per_run_nanos)
      |> Ordered_strings_map.of_alist_exn
    in
    (comp_name |> String.uncapitalize, i), results)
  |> Map.of_alist_exn (module Ordered_strings_map.Key)
  |> Map.transpose_keys (module Ordered_strings_map.Key)
  |> Map.map ~f:Map.to_alist
  |> Map.to_alist
  |> List.map ~f:(fun ((scenario, _), result) ->
    let result = List.map result ~f:(fun ((name, _), value) -> name, value) in
    scenario, result)
  |> Expectable.print_alist
       ?separate_rows:print_separate_rows
       [%sexp_of: (string * Time_ns.Span.t) list]
;;

let benchmark_compare_interactions
  ?print_separate_rows
  ?run_config
  ~get_inject
  ~computations
  scenarios
  =
  let tests =
    List.map scenarios ~f:(fun { Scenario.initial; test_name; interaction } computation ->
      let input = Input.create initial in
      create
        ~name:test_name
        ~component:(computation (Input.value input))
        ~get_inject
        (interaction input))
  in
  benchmark_compare ?print_separate_rows ?run_config ~computations ~tests ()
;;

let const_value_not_constant_folded x = Input.(value (create x))

let benchmark_compare_startup ?print_separate_rows ?run_config ~computations inputs =
  let tests =
    List.map inputs ~f:(fun (name, input) computation ->
      create_for_startup ~name (computation (const_value_not_constant_folded input)))
  in
  benchmark_compare ?print_separate_rows ?run_config ~computations ~tests ()
;;
