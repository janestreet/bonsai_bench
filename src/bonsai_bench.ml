open! Core
include Config
open Bonsai_bench_scenario
module Input = Input
module Scenario = Scenario

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
            ~get_inject:Config.startup_get_inject
            ~interaction:Interaction.recompute
            ~filter_profiles:true
        in
        Runner.run_interactions runner ~handle_profile:(Fn.const ());
        Cleanup.schedule runner
    in
    Core_bench_js.Test.create_with_initialization ~name gc_before_run
;;

module For_running_manually = struct
  let measure ?run_config ts =
    ts |> List.map ~f:to_core_bench_test |> Core_bench_js.measure ?run_config
  ;;

  let benchmark
    ?save_measurement
    ?run_config
    ?analysis_configs
    ?display_config
    ?libname
    ts
    =
    let measurements =
      ts
      |> List.map ~f:to_core_bench_test
      |> Core_bench_js.measure ?run_config
      |> List.map ~f:(fun measurement ->
        (match save_measurement with
         | None -> ()
         | Some f -> f measurement);
        measurement)
    in
    Core_bench_js.analyze_and_display
      ~measurements
      ?analysis_configs
      ?display_config
      ?libname
      ()
  ;;

  let profile profiles = List.iter ~f:Profile.profile profiles
end

module Comparison = struct
  type ('input, 'output) t =
    { print_separate_rows : bool option
    ; computations : (string * ('input, 'output) compare_computation) list
    ; tests : (('input, 'output) compare_computation -> Config.t) list
    }

  let run
    ~run_config
    ~analysis_configs
    ~save_measurement
    { print_separate_rows; computations; tests }
    =
    let module Ordered_strings = struct
      type t = string * int [@@deriving sexp]

      let compare (_, a) (_, b) = Int.compare a b
    end
    in
    let module Ordered_strings_map = Map.Make (Ordered_strings) in
    List.mapi computations ~f:(fun i (comparison_config_name, computation) ->
      let maybe_save_to_file measurement =
        (match save_measurement with
         | None -> ()
         | Some save_measurement ->
           save_measurement
             ~comparison_config_name:(Some comparison_config_name)
             measurement);
        measurement
      in
      let results =
        For_running_manually.measure
          ~run_config
          (List.map tests ~f:(fun t -> t computation))
        |> List.map ~f:maybe_save_to_file
        |> List.map ~f:(Core_bench_js.analyze ~analysis_configs)
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
      (comparison_config_name |> String.uncapitalize, i), results)
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
end

module Benchmark_set = struct
  module Benchmarks = struct
    type t =
      | Comparison : ('input, 'output) Comparison.t -> t
      | Set : Config.t list -> t
      | Profile : Config.t list -> t
  end

  type t =
    { name : string
    ; benchmarks : Benchmarks.t
    }
end

let set ~name benchmarks = { Benchmark_set.name; benchmarks = Set benchmarks }
let profile ~name benchmarks = { Benchmark_set.name; benchmarks = Profile benchmarks }
let const_value_not_constant_folded x = Input.(value (create x))

let compare_startup ?print_separate_rows ~name ~computations inputs =
  let tests =
    List.map inputs ~f:(fun (name, input) computation ->
      create_for_startup ~name (computation (const_value_not_constant_folded input)))
  in
  { Benchmark_set.name
  ; benchmarks = Comparison { print_separate_rows; computations; tests }
  }
;;

let compare_interactions ?print_separate_rows ~name ~get_inject ~computations scenarios =
  let tests =
    List.map scenarios ~f:(fun { Scenario.initial; test_name; interaction } computation ->
      let input = Input.create initial in
      create
        ~name:test_name
        ~component:(computation (Input.value input))
        ~get_inject
        (interaction input))
  in
  { Benchmark_set.name
  ; benchmarks = Comparison { print_separate_rows; computations; tests }
  }
;;

let already_ran_command_at = ref None

let run_command ~here ~list_benchmarks ~count_benchmarks ~run_benchmarks =
  let command =
    let open Command.Let_syntax in
    Core_bench_internals.Bench_command.make_ext
      ~filename_argtype:Command.Arg_type.Export.string
      ~summary:(sprintf "run bonsai benchmarks")
      [%map_open
        let list_only =
          flag
            "list-only"
            no_arg
            ~doc:" List benchmarks & profiles and exit without running them."
          (* [count_benchmarks_only] doesn't consider the number of profiles, because
             they aren't affected by the quota, and we want to use the count to suggest
             quotas / timeouts based on each other. *)
        and count_benchmarks_only =
          flag
            "count-benchmarks-only"
            no_arg
            ~doc:" Print # of benchmarks (not profiles) and exit without running them."
        and machine_output_file =
          flag
            "-machine-output-file"
            (optional string)
            ~doc:
              "MACHINE_OUTPUT_FILE Dump serialized measurements as a \
               [Bonsai_bench_protocol.Machine_output.t] to this file."
        in
        fun (analysis_configs, display_config, what_to_do) ->
          (match !already_ran_command_at with
           | Some already_ran_command_at ->
             raise_s
               [%message
                 "Only one usage of [run_via_command] / [run_sets_via_command] may be \
                  used per executable."
                   (here : Source_code_position.t)
                   (already_ran_command_at : Source_code_position.t)]
           | None -> already_ran_command_at := Some here);
          if list_only && count_benchmarks_only
          then failwith "Can't use both --list-only and --count-benchmarks-only."
          else if list_only
          then list_benchmarks ()
          else if count_benchmarks_only
          then count_benchmarks () |> Int.to_string |> print_endline
          else (
            match what_to_do with
            | `Run (_save_to_file, run_config) ->
              let run_config =
                { (run_config : Core_bench_internals.Run_config.t) with
                  quota = Span (Time_float.Span.of_sec 1.0)
                }
              in
              (match machine_output_file with
               | None ->
                 run_benchmarks
                   ~run_config
                   ~analysis_configs
                   ~display_config
                   ~save_measurement:None
               | Some machine_output_file ->
                 (try Out_channel.write_all machine_output_file ~data:"" with
                  | exn ->
                    raise_s
                      [%message
                        "Could not write to machine output file; does it exist and is it \
                         writable?"
                          (machine_output_file : string)
                          (exn : Exn.t)]);
                 let measurements = ref [] in
                 let save_measurement ~comparison_config_name measurement =
                   measurements
                   := { Bonsai_bench_protocol.Machine_output.V1.Measurement.dimensions =
                          { benchmark_name =
                              Core_bench_internals.Measurement.name measurement
                          ; comparison_config_name
                          ; backend =
                              (match Sys.backend_type with
                               | Native -> "native"
                               | Bytecode -> "bytecode"
                               | Other other ->
                                 (* Js_of_ocaml and Wasm_of_ocaml fall into this group. *)
                                 other)
                          ; tags = String.Map.empty
                          }
                      ; samples =
                          Core_bench_internals.Measurement.samples measurement
                          |> Array.to_list
                      }
                      :: !measurements
                 in
                 run_benchmarks
                   ~run_config
                   ~analysis_configs
                   ~display_config
                   ~save_measurement:(Some save_measurement);
                 let serialized =
                   Bonsai_bench_protocol.Machine_output.sexp_of_t
                     (V1 { measurements = !measurements })
                   |> Sexp.to_string_mach
                 in
                 Out_channel.write_all machine_output_file ~data:serialized)
            | `From_file _filenames ->
              failwith "Loading saved files is not supported for inline executables.")]
  in
  Command_nodejs.run command
;;

let list_benchmarks ?(prefix = "") benchmarks =
  List.iter benchmarks ~f:(function
    | Startup { name; _ } -> print_endline [%string "%{prefix}(Startup) %{name}"]
    | Interactions { name; _ } ->
      print_endline [%string "%{prefix}(Interactions) %{name}"])
;;

let run_via_command ~(here : [%call_pos]) benchmarks =
  let run_benchmarks ~run_config ~analysis_configs ~display_config ~save_measurement =
    For_running_manually.benchmark
      ~run_config
      ~analysis_configs
      ~display_config
      ?save_measurement:
        (let%map.Option save_measurement in
         save_measurement ~comparison_config_name:None)
      benchmarks
  in
  run_command
    ~here
    ~list_benchmarks:(fun () -> list_benchmarks benchmarks)
    ~count_benchmarks:(fun () -> List.length benchmarks)
    ~run_benchmarks
;;

let print_set_name name = print_endline [%string "===== %{name} ====="]

let run_sets_via_command ~(here : [%call_pos]) sets =
  let list_benchmarks () =
    List.iter sets ~f:(fun { Benchmark_set.name; benchmarks } ->
      print_set_name name;
      (match benchmarks with
       | Set benchmarks -> list_benchmarks benchmarks
       | Profile benchmarks -> list_benchmarks ~prefix:"(Profile) " benchmarks
       | Comparison { computations; tests; _ } ->
         let computation_names = List.map computations ~f:fst in
         (match computations with
          | (_, arbitrary_computation) :: _ ->
            let benchmark_names =
              List.map tests ~f:(fun f ->
                match f arbitrary_computation with
                | Startup { name; _ } -> name
                | Interactions { name; _ } -> name)
            in
            let paired, rem = List.zip_with_remainder computation_names benchmark_names in
            let rem_fixed =
              match rem with
              | Some (First x) -> List.map x ~f:(fun x -> x, "")
              | Some (Second x) -> List.map x ~f:(fun x -> "", x)
              | None -> []
            in
            List.map (paired @ rem_fixed) ~f:(fun (computation_name, benchmark_name) ->
              [%sexp { computation_name : string; benchmark_name : string }])
            |> Expectable.print
          | _ -> raise_s [%message "BUG: no computations provided to comparison"]));
      print_endline "")
  in
  let count_benchmarks () =
    List.fold sets ~init:0 ~f:(fun count { benchmarks; _ } ->
      match benchmarks with
      | Set benchmarks -> count + List.length benchmarks
      | Profile _ -> count
      | Comparison { computations; tests; _ } ->
        count + (List.length computations * List.length tests))
  in
  let run_benchmarks ~run_config ~analysis_configs ~display_config ~save_measurement =
    List.iter sets ~f:(fun { Benchmark_set.name; benchmarks } ->
      print_set_name name;
      match benchmarks with
      | Set benchmarks ->
        For_running_manually.benchmark
          ~run_config
          ~analysis_configs
          ~display_config
          ?save_measurement:
            (let%map.Option save_measurement in
             save_measurement ~comparison_config_name:None)
          benchmarks
      | Profile configs -> For_running_manually.profile configs
      | Comparison comparison ->
        Comparison.run ~run_config ~analysis_configs comparison ~save_measurement)
  in
  run_command ~here ~list_benchmarks ~count_benchmarks ~run_benchmarks
;;

module For_testing = For_running_manually
