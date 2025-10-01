open! Core
open Bonsai.For_open
open! Bonsai.Let_syntax
open! Bonsai_bench
module Perf_configs = Bonsai_test_shared_for_testing_bonsai.Perf_configs

module Basic_benchmarking = struct
  let const =
    Bonsai_bench.create
      ~name:"Bonsai.const"
      ~component:(fun (local_ _graph) -> Bonsai.return 4)
      ~get_inject:(fun _ -> Nothing.unreachable_code)
      (Interaction.many [])
  ;;

  let state =
    Bonsai_bench.create
      ~name:"Bonsai.state"
      ~component:(fun (local_ graph) ->
        let state, set_state =
          Bonsai.state 0 ~sexp_of_model:[%sexp_of: Int.t] ~equal:[%equal: Int.t] graph
        in
        let%arr state and set_state in
        state, set_state)
      ~get_inject:(fun (_, inject) -> inject)
      Interaction.(many_with_recomputes [ inject 1; reset_model ])
  ;;

  module State_machine = struct
    module Action = struct
      type t =
        | Incr
        | Decr
      [@@deriving sexp, equal]
    end

    let component (local_ graph) =
      let state, inject =
        Bonsai.state_machine
          graph
          ~sexp_of_model:[%sexp_of: Int.t]
          ~equal:[%equal: Int.t]
          ~sexp_of_action:[%sexp_of: Action.t]
          ~default_model:0
          ~apply_action:(fun (_ : _ Bonsai.Apply_action_context.t) model action ->
            match action with
            | Incr -> model + 1
            | Decr -> model - 1)
      in
      let%arr state and inject in
      state, inject
    ;;

    let incr = Interaction.inject Action.Incr
    let decr = Interaction.inject Action.Decr
  end

  (* The state does not get reset by default between benchmark runs. We choose
   an interaction which is idempotent so each test run will be identical. *)
  let state_machine_idempotent =
    [ State_machine.incr; State_machine.decr; State_machine.incr; State_machine.decr ]
    |> Interaction.many_with_recomputes
    |> Bonsai_bench.create
         ~name:"Bonsai.state_machine0: idempotent"
         ~component:State_machine.component
         ~get_inject:(fun (_, inject) -> inject)
  ;;

  (* This state machine benchmark does not have an idempotent interaction. As a result, this
   test will cause the model to grow by 2 every time the test is run during benchmarking.
   Since (number of test runs * 2) won't exceed the range of int and the performance of
   incrementing/decrementing by 1 is ~identical between int values, this won't cause
   skewed benchmark results. *)
  let state_machine_without_reset =
    [ State_machine.incr; State_machine.decr; State_machine.incr; State_machine.incr ]
    |> Interaction.many_with_recomputes
    |> Bonsai_bench.create
         ~name:"Bonsai.state_machine0: not idempotent; no model reset"
         ~component:State_machine.component
         ~get_inject:(fun (_, inject) -> inject)
  ;;

  (* If it was important that the state machine restarted from the same model every time,
   then we could use [Test.create_with_resetter] to explicitly reset the model. This
   comes with an overhead cost, though, as we must reset the model and perform a
   stabilization. Note that the interaction performed in [state_machine_without_reset]
   and [state_machine_with_reset] are identical. *)
  let state_machine_with_reset =
    [ State_machine.incr; State_machine.decr; State_machine.incr; State_machine.incr ]
    |> Interaction.many_with_recomputes
    |> Bonsai_bench.create_with_resetter
         ~name:
           "Bonsai.state_machine0: not idempotent; model reset using create_with_resetter"
         ~component:State_machine.component
         ~get_inject:(fun (_, inject) -> inject)
  ;;

  (* We can also manually reset the component's model with [Interaction.reset_model]. The
   test above with [Test.create_with_resetter] is equivalent to this one. *)
  let state_machine_with_manual_reset =
    [ State_machine.incr
    ; State_machine.decr
    ; State_machine.incr
    ; State_machine.incr
    ; Interaction.reset_model
    ; Interaction.recompute
    ]
    |> Interaction.many_with_recomputes
    |> Bonsai_bench.create
         ~name:"Bonsai.state_machine0: not idempotent; model reset manually"
         ~component:State_machine.component
         ~get_inject:(fun (_, inject) -> inject)
  ;;

  module My_triple = struct
    let component first second third (local_ _graph) =
      let%arr first and second and third in
      first, second, third
    ;;
  end

  (* This benchmark calls stabilize in between setting each of the components in the
   [My_triple] component. *)
  let piecewise_triple_stabilize_between_each =
    let first = Input.create 0 in
    let second = Input.create "" in
    let third = Input.create 0. in
    Interaction.
      [ change_input first 1
      ; change_input second "second"
      ; change_input third 3.
      ; change_input first 0
      ; change_input second ""
      ; change_input third 0.
      ]
    |> Interaction.many_with_recomputes
    |> Bonsai_bench.create
         ~name:"My_triple setting components and stabilizing between each one"
         ~component:
           (My_triple.component
              (Input.value first)
              (Input.value second)
              (Input.value third))
         ~get_inject:(fun _ -> Nothing.unreachable_code)
  ;;

  (* If we wanted to ensure stabilization only happened after all of the inputs were set,
   we could do the following. Since [many_with_recomputes] just intersperses
   [stabilize]s in the list of interactions, stabilization is only inserted between the
   two [many] groups below. *)
  let piecewise_triple_stabilize_after_all =
    let first = Input.create 0 in
    let second = Input.create "" in
    let third = Input.create 0. in
    Interaction.
      [ many [ change_input first 1; change_input second "second"; change_input third 3. ]
      ; many [ change_input first 0; change_input second ""; change_input third 0. ]
      ]
    |> Interaction.many_with_recomputes
    |> Bonsai_bench.create
         ~name:"My_triple setting components and stabilizing after all three"
         ~component:
           (My_triple.component
              (Input.value first)
              (Input.value second)
              (Input.value third))
         ~get_inject:(fun _ -> Nothing.unreachable_code)
  ;;

  module Do_work_every_second = struct
    let component (local_ graph) =
      Bonsai.Incr.with_clock
        ~f:(fun clock ->
          let%map.Ui_incr () =
            Bonsai.Time_source.at_intervals clock (Time_ns.Span.of_sec 1.0)
          in
          for _ = 1 to 1000 do
            ()
          done)
        graph
    ;;

    let advance_by_zero = Interaction.advance_clock_by (Time_ns.Span.of_sec 0.)
    let advance_by_second = Interaction.advance_clock_by (Time_ns.Span.of_sec 1.0)
  end

  let do_work_every_second_advance_by_zero =
    Bonsai_bench.create
      ~name:"Component that does work every second: advance clock by zero each run"
      ~component:Do_work_every_second.component
      ~get_inject:(fun _ -> Nothing.unreachable_code)
      Do_work_every_second.advance_by_zero
  ;;

  let do_work_every_second_advance_by_second =
    Bonsai_bench.create
      ~name:"Component that does work every second: advance clock by a second each run"
      ~component:Do_work_every_second.component
      ~get_inject:(fun _ -> Nothing.unreachable_code)
      Do_work_every_second.advance_by_second
  ;;

  let two_state_machines_that_alternate =
    let component (local_ graph) =
      let which, set_which =
        Bonsai.state true ~sexp_of_model:[%sexp_of: Bool.t] ~equal:[%equal: Bool.t] graph
      in
      let%arr which
      and set_which
      and state_1, inject_1 = State_machine.component graph
      and state_2, inject_2 = State_machine.component graph in
      let inject action =
        match which with
        | true -> Effect.Many [ set_which false; inject_1 action ]
        | false -> Effect.Many [ set_which true; inject_2 action ]
      in
      (state_1, state_2), inject
    in
    [ State_machine.incr; State_machine.incr; State_machine.decr; State_machine.decr ]
    |> Interaction.many_with_recomputes
    |> Bonsai_bench.create
         ~name:"Alternating state machines"
         ~component
         ~get_inject:(fun (_, inject) -> inject)
  ;;

  (* These computations will all be benchmarked by [Core_bench_js]. *)
  let benchmarks =
    [ const
    ; state
    ; state_machine_idempotent
    ; state_machine_without_reset
    ; state_machine_with_reset
    ; state_machine_with_manual_reset
    ; piecewise_triple_stabilize_after_all
    ; piecewise_triple_stabilize_between_each
    ; do_work_every_second_advance_by_zero
    ; do_work_every_second_advance_by_second
    ; two_state_machines_that_alternate
    ]
    |> Bonsai_bench.set ~name:"Basic Benchmarking"
  ;;
end

module Benchmarking_computation_with_bug = struct
  (* Sometimes, you may notice that a benchmark is suspiciously slow. In that case, it may
   be helpful to [profile] the computation to see what's taking so long. For example,
   consider the following: *)

  type r =
    { a : int
    ; b : Time_ns.t
    }

  let do_some_work a =
    for _ = 1 to a do
      Sys.opaque_identity ()
    done
  ;;

  let component_that_does_work_too_often =
    let component (local_ graph) =
      let now = Bonsai.Clock.now graph in
      let r =
        let%arr now in
        { a = 1000000; b = now }
      in
      (* BUG: The below [Bonsai.map] will get fired every time any field in the record changes,
       i.e, whenever [Bonsai.Clock.now] updates. The body of this is expensive and depends
       only on [a]. *)
      Bonsai.map r ~f:(fun { a; _ } -> do_some_work a)
    in
    Interaction.advance_clock_by (Time_ns.Span.of_ms 1.)
    |> Bonsai_bench.create
         ~name:"Component that fires too often"
         ~component
         ~get_inject:(fun _ -> Nothing.unreachable_code)
  ;;

  let component_that_does_work_the_right_amount =
    let component (local_ graph) =
      let now = Bonsai.Clock.now graph in
      let r =
        let%arr now in
        { a = 1000000; b = now }
      in
      (* This [let%sub] ensures that the below [let%arr] only depends on [a], and hence
       doesn't run when [Bonsai.Clock.now] updates. *)
      let%sub { a; _ } = r in
      let%arr a in
      do_some_work a
    in
    Interaction.advance_clock_by (Time_ns.Span.of_ms 1.)
    |> Bonsai_bench.create
         ~name:"Component that fires the right amount"
         ~component
         ~get_inject:(fun _ -> Nothing.unreachable_code)
  ;;

  let benchmarks =
    [ component_that_does_work_too_often; component_that_does_work_the_right_amount ]
    |> Bonsai_bench.set ~name:"Benchmarking a computation with a bug"
  ;;

  let profile =
    Bonsai_bench.profile
      ~name:"Profiling computations to find the bug"
      [ component_that_does_work_too_often; component_that_does_work_the_right_amount ]
  ;;
end

module Comparison_benchmarking_list_assoc = struct
  let startup =
    Bonsai_bench.compare_startup
      ~name:"Startup: Map vs Assoc"
      ~computations:(force Perf_configs.Dynamic_num.all_computations)
      (force Perf_configs.Dynamic_num.startup_inputs)
  ;;

  let interactions =
    Bonsai_bench.compare_interactions
      ~name:"Interactions: Map vs Assoc"
      ~get_inject:(fun _ _ -> Effect.Ignore)
      ~computations:(force Perf_configs.Dynamic_num.all_computations)
      (force Perf_configs.Dynamic_num.scenarios)
  ;;
end

module Comparison_benchmarking_match = struct
  let startup =
    Bonsai_bench.compare_startup
      ~name:"Startup: let%arr + match vs match%sub"
      ~computations:(force Perf_configs.Switch.all_computations)
      Perf_configs.Switch.startup_inputs
  ;;

  let interactions =
    Bonsai_bench.compare_interactions
      ~name:"Interactions: let%arr + match vs match%sub"
      ~get_inject:(fun _ _ -> Effect.Ignore)
      ~computations:(force Perf_configs.Switch.all_computations)
      Perf_configs.Switch.scenarios
  ;;
end

let () =
  Bonsai_bench.run_sets_via_command
    [ Basic_benchmarking.benchmarks
    ; Benchmarking_computation_with_bug.benchmarks
    ; Benchmarking_computation_with_bug.profile
    ; Comparison_benchmarking_list_assoc.startup
    ; Comparison_benchmarking_list_assoc.interactions
    ; Comparison_benchmarking_match.startup
    ; Comparison_benchmarking_match.interactions
    ]
;;
