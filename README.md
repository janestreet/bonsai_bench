# Benchmarking Bonsai Components

`Bonsai_bench` is a library which provides facilities for benchmarking Bonsai components.

# Creating a benchmark

`Bonsai_bench.Test.create` creates a benchmark. It takes a component to benchmark, a way
to extract that component's `inject_action` function, and a list of interactions to
perform during the benchmark. For example:

```ocaml
let state =
  Bonsai_bench.Test.create
    ~name:"Bonsai.state"
    ~component:(Bonsai.state [%here] (module Int) ~default_model:0)
    ~get_inject:(fun (_, inject) -> inject)
    Bonsai_bench.Interaction.(many_with_recomputes [ inject 1; reset_model ])
```

There are more examples in the `example/` directory.

# Running a benchmark

To run the benchmark, pass it to `Bonsai_bench.bench`, a thin wrapper around
`Core_bench_js.bench` that handles necessary cleanup between tests:

```ocaml
let () =
  let quota = Core_bench_js.Quota.Span (Time.Span.of_sec 1.0) in
  Bonsai_bench.bench ~run_config:(Core_bench_js.Run_config.create () ~quota) [ state ]
```

Build the `javascript-executables` target for the directory with `BUILD_PROFILE=fast-exe`.
Then, from a terminal, run: `node path/to/executable/name.bc.js` to see the results.


<!--
  --expose-gc       : allows you to run a garbage collection using [gc()] at any point
  --gc-global       : will perform a stop-the-world global garbage collection
  --gc-interval <n> : will gc after <n> allocations.  Setting this very high might let
                      us prevent garbage collection during test runs.
  --always-compact  : force compaction during gc
  --random-seed <n> : seed to use for randomness
-->

# Code organization

- `src/bonsai_bench.mli`: Contains the API for running and creating tests. The list of
  possible interactions that can be performed during benchmarks can also be found there.
