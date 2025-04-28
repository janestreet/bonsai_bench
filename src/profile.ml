open! Core
module Graph_info = Bonsai.Private.Graph_info

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

module Measurement = struct
  module Kind = struct
    module T = struct
      type t =
        | Startup
        | Snapshot
        | Named of string
      [@@deriving sexp, equal, compare]
    end

    include T
    include Comparable.Make_plain (T)

    let time_to_first_stabilization = "Bonsai_bench profile: first stabilization"
    let time_since_snapshot_began = "Bonsai_bench profile: current snapshot"

    let to_string = function
      | Named s -> s
      | Startup -> time_to_first_stabilization
      | Snapshot -> time_since_snapshot_began
    ;;

    let is_bonsai_measurement = function
      | Named _ -> true
      | _ -> false
    ;;
  end

  type t =
    { kind : Kind.t
    ; duration : Time_ns.Span.t
    ; id : int option
    }
  [@@deriving sexp]

  let create kind timer =
    match Javascript_profiling.Timer.(stop timer |> duration) with
    | Ok duration -> { kind; duration; id = None }
    | Backgrounding_changed_unreliable _duration ->
      failwith "BUG: Backgrounding should not have changed while benchmarking."
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
    | Named _, Named _ -> Time_ns.Span.descending total_duration total_duration'
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

let create_summary_table ~total_time ~incremental_time =
  let incremental_overhead = Time_ns.Span.(total_time - incremental_time) in
  let open Ascii_table_kernel in
  to_string_noattr
    [ Column.create "Statistic" fst; Column.create "Value" snd ]
    ~limit_width_to:Int.max_value
    ~bars:`Unicode
    [ "Total time", Time_ns.Span.to_string_hum total_time
    ; "Incremental time", Time_ns.Span.to_string_hum incremental_time
    ; "Incremental Overhead", Time_ns.Span.to_string_hum incremental_overhead
    ; "Incremental Overhead (%)", spans_pct incremental_overhead total_time
    ]
;;

let create_snapshot_table data ~incremental_time =
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
        "Percent of incremental time"
        (fun { Accumulated_measurement.total_duration; _ } ->
           spans_pct total_duration incremental_time)
    ]
  in
  to_string_noattr columns data ~limit_width_to:Int.max_value ~bars:`Unicode
;;

let print_statistics data =
  let sorted_data = List.sort (Map.data data) ~compare:Accumulated_measurement.compare in
  let incremental_measurements, bonsai_bench_internals =
    List.partition_tf sorted_data ~f:(fun { kind; _ } ->
      Measurement.Kind.is_bonsai_measurement kind)
  in
  let total_time =
    match bonsai_bench_internals with
    | [ { Accumulated_measurement.total_duration; _ } ] -> total_duration
    | _ ->
      raise_s
        [%message
          "An error occurred while profiling your computation. Bonsai bench expected \
           only one internal measurement. Please report this error to the bonsai team."
            ~internal_measurements:
              (bonsai_bench_internals : Accumulated_measurement.t list)]
  in
  let incremental_time =
    List.sum
      (module Time_ns.Span)
      incremental_measurements
      ~f:(fun { kind; total_duration; _ } ->
        if Measurement.Kind.is_bonsai_measurement kind
        then total_duration
        else Time_ns.Span.zero)
  in
  print_endline "Summary:";
  print_endline (create_summary_table ~total_time ~incremental_time);
  print_endline "Details:";
  print_endline (create_snapshot_table incremental_measurements ~incremental_time)
;;

let accumulate_measurements
  ~(source_locations : Graph_info.Node_info.t Bonsai.Private.Node_path.Map.t)
  measurements
  =
  let with_ids, without_ids =
    List.map measurements ~f:(fun measurement ->
      match measurement.Measurement.kind with
      | Snapshot | Startup -> measurement
      | Named label ->
        Option.value
          ~default:measurement
          (let%bind.Option node_path =
             Bonsai.Private.Instrumentation.extract_node_path_from_entry_label label
           in
           let%bind.Option { node_type; here } = Map.find source_locations node_path in
           let%map.Option here in
           { measurement with
             kind = Named [%string "%{node_type} (%{here#Source_code_position})"]
           ; id = Some (Id.of_node_path node_path)
           }))
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

let take_profile_snapshot ~name graph_info performance_entries =
  match List.length !performance_entries with
  | 0 | 1 -> ()
  | _ ->
    print_endline [%string "Bonsai_bench Profile: %{name}"];
    let source_locations =
      Graph_info.pull_source_locations_from_nearest_parent graph_info
    in
    print_statistics (accumulate_measurements ~source_locations !performance_entries);
    performance_entries := []
;;

let profile = function
  | Config.Startup _ -> print_endline "Profiling startup benchmarks is not supported."
  | Interactions { time_source; component; get_inject; interaction; name } ->
    print_endline [%string "Running Bonsai_bench profile of %{name}"];
    let graph_info = ref Graph_info.empty in
    let performance_entries = ref [] in
    let store_entry entry = performance_entries := entry :: !performance_entries in
    let snapshot_timer = ref None in
    let handle_profile name =
      Option.iter !snapshot_timer ~f:(fun timer ->
        Measurement.create Snapshot timer |> store_entry);
      take_profile_snapshot ~name !graph_info performance_entries;
      snapshot_timer := Some (Javascript_profiling.Timer.start ())
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
                match evt with
                | Profiling_entry s -> Measurement.create (Named s) timer |> store_entry
                | _ -> (* We only care about profiling measurements. *) ())
          }
        ~wrap_driver_creation:
          { f =
              (fun create_driver ->
                let timer = Javascript_profiling.Timer.start () in
                let driver = create_driver () in
                Measurement.create Startup timer |> store_entry;
                driver)
          }
        ~time_source
        ~component
        ~get_inject
        ~interaction
    in
    take_profile_snapshot ~name:"startup" !graph_info performance_entries;
    snapshot_timer := Some (Javascript_profiling.Timer.start ());
    Runner.run_interactions runner ~handle_profile;
    Runner.invalidate_observers runner
;;
