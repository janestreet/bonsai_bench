open! Core
open Bonsai
open! Bonsai.Let_syntax
open! Bonsai_bench

let assoc_with_states ~input_size =
  let computation graph =
    let inputs =
      List.init input_size ~f:(fun i -> i, i) |> Int.Map.of_alist_exn |> Bonsai.return
    in
    let assoc =
      Bonsai.assoc
        (module Int)
        inputs
        ~f:(fun _key data graph ->
          let state, _ = Bonsai.state 0 graph in
          let%arr state and data in
          state + data)
        graph
    in
    Bonsai.Map.sum assoc (module Int) ~f:Fn.id graph
  in
  Bonsai_bench.create_for_startup
    ~name:[%string "Assoc with %{input_size#Int} constant inputs"]
    computation
;;

let benches =
  lazy
    [ assoc_with_states ~input_size:1
    ; assoc_with_states ~input_size:5
    ; assoc_with_states ~input_size:10
    ; assoc_with_states ~input_size:20
    ; assoc_with_states ~input_size:50
    ; assoc_with_states ~input_size:100
    ; assoc_with_states ~input_size:1_000
    ; assoc_with_states ~input_size:10_000
    ]
;;

let () = print_endline "======== Benchmarking Startup ========"

let () =
  let quota = Core_bench_js.Quota.Span (Time_float.Span.of_sec 0.1) in
  force benches
  |> Bonsai_bench.benchmark ~run_config:(Core_bench_js.Run_config.create () ~quota)
;;

let () = print_endline "======== Profiling Startup ========"
let () = Bonsai_bench.profile (force benches)
