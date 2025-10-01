open! Core
open Bonsai
open! Bonsai.Let_syntax
open! Bonsai_bench

let assoc_with_states ~input_size =
  let computation (local_ graph) =
    let inputs =
      List.init input_size ~f:(fun i -> i, i) |> Int.Map.of_alist_exn |> Bonsai.return
    in
    let assoc =
      Bonsai.assoc
        (module Int)
        inputs
        ~f:(fun _key data (local_ graph) ->
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

let () =
  Bonsai_bench.run_sets_via_command
    [ Bonsai_bench.set ~name:"Benchmarking Startup" benches
    ; Bonsai_bench.profile ~name:"Benchmarking Startup" benches
    ]
;;
