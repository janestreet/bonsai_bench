open! Core
module Graph_info = Bonsai.Private.Graph_info
module Interaction = Bonsai_bench_scenario.Interaction

module Id = struct
  let instance = String.Table.create ()

  let of_node_path node_path =
    let key = Bonsai.Private.Node_path.to_string node_path in
    match Hashtbl.find instance key with
    | Some id -> id
    | None ->
      let id = Hashtbl.length instance in
      Hashtbl.set instance ~key ~data:id;
      id
  ;;
end

let stop_timer timer =
  match Javascript_profiling.Timer.(stop timer |> duration) with
  | Ok duration -> duration
  | Backgrounding_changed_unreliable _duration ->
    failwith "BUG: Backgrounding should not have changed while benchmarking."
;;

module Measurement = struct
  module Kind = struct
    module T = struct
      type t =
        | Graph_application
        | Preprocess
        | Gather
        | Run_eval_fun
        | First_stabilization
        | Incremental_recompute of string
        | Stabilize_for_clock
        | Apply_actions
        | Stabilize_for_action
        | Stabilize_after_all_apply_actions
      [@@deriving sexp, equal, compare]

      let is_incremental_recompute = function
        | Incremental_recompute _ -> true
        | _ -> false
      ;;

      let is_stabilize = function
        | First_stabilization
        | Stabilize_for_clock
        | Stabilize_for_action
        | Stabilize_after_all_apply_actions -> true
        | _ -> false
      ;;
    end

    include T
    include Comparable.Make_plain (T)

    (* We don't record while printing results, so allocating strings in here should be
       fine. *)
    let to_string = function
      | Incremental_recompute label -> label
      | Graph_application -> "[startup] Graph Application"
      | Preprocess -> "[startup] Preprocess"
      | Gather -> "[startup] Gather"
      | Run_eval_fun -> "[startup] Run Eval Fun"
      | First_stabilization -> "[startup] First Stabilization"
      | Stabilize_for_clock -> "[runtime] Stabilize For Clock"
      | Apply_actions -> "[runtime] Apply Actions"
      | Stabilize_for_action -> "[runtime] Stabilize For Action"
      | Stabilize_after_all_apply_actions -> "[runtime] Stabilize After All Apply Actions"
    ;;
  end

  type t =
    { kind : Kind.t
    ; duration : Time_ns.Span.t
    ; id : int option
    }
  [@@deriving sexp]

  let create kind ~duration = { kind; duration; id = None }

  let annotate
    ~(source_locations : Graph_info.Node_info.t Bonsai.Private.Node_path.Map.t)
    measurement
    =
    match measurement.kind with
    | Incremental_recompute label ->
      let with_source_position =
        let%bind.Option node_path =
          Bonsai.Private.Instrumentation.extract_node_path_from_entry_label label
        in
        let%bind.Option { node_type; here } = Map.find source_locations node_path in
        let%map.Option here in
        { measurement with
          kind =
            Incremental_recompute [%string "%{node_type} (%{here#Source_code_position})"]
        ; id = Some (Id.of_node_path node_path)
        }
      in
      Option.value with_source_position ~default:measurement
    | Graph_application
    | Preprocess
    | Gather
    | Run_eval_fun
    | First_stabilization
    | Stabilize_for_clock
    | Apply_actions
    | Stabilize_for_action
    | Stabilize_after_all_apply_actions -> measurement
  ;;
end

module Accumulated_measurement = struct
  type t =
    { kind : Measurement.Kind.t
    ; total_duration : Time_ns.Span.t
    ; count : int
    ; id : int option
    }
  [@@deriving sexp]

  let compare
    { kind; total_duration; _ }
    { kind = kind'; total_duration = total_duration'; _ }
    =
    match kind, kind' with
    | Incremental_recompute _, Incremental_recompute _ ->
      Time_ns.Span.descending total_duration total_duration'
    | _, _ -> Measurement.Kind.compare kind kind'
  ;;

  let of_measurement { Measurement.kind; duration; id } =
    { kind; count = 1; total_duration = duration; id }
  ;;

  let add { kind; total_duration; count; id } ~measurement =
    assert (Measurement.Kind.equal kind measurement.Measurement.kind);
    { kind
    ; count = count + 1
    ; total_duration = Time_ns.Span.(total_duration + measurement.duration)
    ; id
    }
  ;;
end

let spans_pct a b =
  Percent.Always_percentage.to_string
    (Percent.of_percentage (Time_ns.Span.(to_ns a /. to_ns b) *. 100.))
;;

let create_summary_table ~total_time ~let_arr_time ~stabilize_time =
  let other_time = Time_ns.Span.(total_time - let_arr_time) in
  let open Ascii_table_kernel in
  to_string_noattr
    [ Column.create "Statistic" fst; Column.create "Value" snd ]
    ~limit_width_to:Int.max_value
    ~bars:`Unicode
    [ "Total time", Time_ns.Span.to_string_hum total_time
    ; "Stabilize time", Time_ns.Span.to_string_hum stabilize_time
    ; "let%arr time", Time_ns.Span.to_string_hum let_arr_time
    ; "let%arr / stabilize (%)", spans_pct let_arr_time stabilize_time
    ; "Non let%arr time", Time_ns.Span.to_string_hum other_time
    ; "Non let%arr time (%)", spans_pct other_time total_time
    ]
;;

let create_snapshot_table data ~total_time ~let_arr_time =
  let open Ascii_table_kernel in
  let columns =
    [ Column.create "Id" (fun { Accumulated_measurement.id; _ } ->
        match id with
        | Some int -> Int.to_string int
        | None -> "N/A")
    ; Column.create "Name" (fun { Accumulated_measurement.kind; _ } ->
        Measurement.Kind.to_string kind)
    ; Column.create "Times fired" (fun { Accumulated_measurement.count; _ } ->
        Int.to_string count)
    ; Column.create "Total time" (fun { Accumulated_measurement.total_duration; _ } ->
        Time_ns.Span.to_string_hum total_duration)
    ; Column.create
        "% of let%arr time"
        (fun { Accumulated_measurement.total_duration; kind; _ } ->
           match Measurement.Kind.is_incremental_recompute kind with
           | true -> spans_pct total_duration let_arr_time
           | false -> "")
    ; Column.create
        "% of total time"
        (fun { Accumulated_measurement.total_duration; _ } ->
           spans_pct total_duration total_time)
    ]
  in
  to_string_noattr columns data ~limit_width_to:Int.max_value ~bars:`Unicode
;;

let print_statistics ~duration data =
  let sorted_measurements =
    List.sort (Map.data data) ~compare:Accumulated_measurement.compare
  in
  let let_arr_time, stabilize_time =
    List.fold
      ~init:(Time_ns.Span.zero, Time_ns.Span.zero)
      sorted_measurements
      ~f:(fun (let_arr_acc, stabilize_acc) { kind; total_duration; _ } ->
        let add_if ~f base =
          if f kind then Time_ns.Span.(base + total_duration) else base
        in
        ( add_if ~f:Measurement.Kind.is_incremental_recompute let_arr_acc
        , add_if ~f:Measurement.Kind.is_stabilize stabilize_acc ))
  in
  print_endline "Summary:";
  print_endline (create_summary_table ~total_time:duration ~let_arr_time ~stabilize_time);
  print_endline "Details:";
  print_endline
    (create_snapshot_table sorted_measurements ~total_time:duration ~let_arr_time)
;;

let accumulate_measurements ~source_locations measurements =
  let with_ids, without_ids =
    List.map measurements ~f:(Measurement.annotate ~source_locations)
    |> List.fold
         ~init:(Int.Map.empty, Measurement.Kind.Map.empty)
         ~f:(fun (with_ids, without_ids) measurement ->
           let accumulate_measurements = function
             | None -> Accumulated_measurement.of_measurement measurement
             | Some accumulated -> Accumulated_measurement.add accumulated ~measurement
           in
           match measurement.id with
           | None ->
             with_ids, Map.update without_ids measurement.kind ~f:accumulate_measurements
           | Some id -> Map.update with_ids id ~f:accumulate_measurements, without_ids)
  in
  (* Assign measurements without IDs (e.g. startup) a new ID. *)
  Map.fold without_ids ~init:with_ids ~f:(fun ~key:_ ~data:measurement acc ->
    let id =
      match Map.max_elt acc with
      (* This could happen if the user never [let%sub]s. It's not very realistic for a
         practical app, but totally possible to write. *)
      | None -> 0
      | Some (id, _) -> id + 1
    in
    let measurement = { measurement with id = Some id } in
    Map.set acc ~key:id ~data:measurement)
;;

let take_profile_snapshot ~duration ~name graph_info recorded_measurements =
  match !recorded_measurements with
  | [] ->
    print_endline [%string "Not printing profile of %{name} because nothing happened"]
  | _ ->
    print_endline [%string "Bonsai_bench Profile: %{name}"];
    let source_locations =
      Graph_info.pull_source_locations_from_nearest_parent graph_info
    in
    print_statistics
      ~duration
      (accumulate_measurements ~source_locations !recorded_measurements);
    recorded_measurements := []
;;

let profile' ~time_source ~get_inject ~interaction ~component =
  let graph_info = ref Graph_info.empty in
  let recorded_measurements = ref [] in
  let store_entry entry = recorded_measurements := entry :: !recorded_measurements in
  let timer_since_last_snapshot = ref None in
  let handle_profile name =
    match !timer_since_last_snapshot with
    | None ->
      raise_s
        [%message
          "Bonsai_bench.profile BUG! No interactions should have been run prior to \
           startup."
            ([%here] : Source_code_position.t)]
    | Some timer ->
      let duration = stop_timer timer in
      take_profile_snapshot ~duration ~name !graph_info recorded_measurements;
      timer_since_last_snapshot := Some (Javascript_profiling.Timer.start ())
  in
  let runner =
    Runner.initialize
      ~filter_profiles:false
      ~driver_instrumentation:
        { instrument_for_computation_watcher =
            Ui_incr.return Bonsai.Private.Instrumentation.Watching.Not_watching
        ; instrument_for_profiling =
            Ui_incr.return Bonsai.Private.Instrumentation.Profiling.Profiling
        ; computation_watcher_queue =
            Queue.create ( (* We don't use the computation watcher. *) )
        ; set_latest_graph_info = (fun gi -> graph_info := gi)
        ; start_timer = (fun evt -> evt, Javascript_profiling.Timer.start ())
        ; stop_timer =
            (fun (evt, timer) ->
              let duration = stop_timer timer in
              let measurement_kind =
                match evt with
                | Profiling_entry s -> Measurement.Kind.Incremental_recompute s
                | Graph_application -> Graph_application
                | Preprocess -> Preprocess
                | Gather -> Gather
                | Run_eval_fun -> Run_eval_fun
                | First_stabilization -> First_stabilization
                | Stabilize_for_clock -> Stabilize_for_clock
                | Apply_actions -> Apply_actions
                | Stabilize_for_action -> Stabilize_for_action
                | Stabilize_after_all_apply_actions -> Stabilize_after_all_apply_actions
              in
              Measurement.create measurement_kind ~duration |> store_entry)
        }
      ~wrap_driver_creation:
        { f =
            (fun create_driver ->
              let timer = Javascript_profiling.Timer.start () in
              let driver = create_driver () in
              let duration = stop_timer timer in
              take_profile_snapshot
                ~duration
                ~name:"startup"
                !graph_info
                recorded_measurements;
              driver)
        }
      ~time_source
      ~component
      ~get_inject
      ~interaction
  in
  if not (List.is_empty !recorded_measurements)
  then
    raise_s
      [%message
        "Bonsai_bench.profile BUG: there should be no profiling entries between \
         profiling creation and running interactions"
          ([%here] : Source_code_position.t)];
  timer_since_last_snapshot := Some (Javascript_profiling.Timer.start ());
  Runner.run_interactions runner ~handle_profile;
  Runner.invalidate_observers runner
;;

let profile = function
  | Config.Startup { time_source; component; name } ->
    print_endline [%string "Running Bonsai_bench startup profile of %{name}"];
    profile'
      ~time_source
      ~get_inject:Config.startup_get_inject
      ~interaction:(Interaction.many [])
      ~component
  | Interactions { time_source; component; get_inject; interaction; name } ->
    print_endline [%string "Running Bonsai_bench profile of %{name}"];
    profile'
      ~time_source
      ~get_inject
      ~interaction:
        (Interaction.many
           [ interaction; Interaction.recompute; Interaction.profile ~name:"end of run" ])
      ~component
;;
