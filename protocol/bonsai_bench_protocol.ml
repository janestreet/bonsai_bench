open! Core

module Dimensions = struct
  module V1 = struct
    type t =
      { benchmark_name : string
      ; comparison_config_name : string option
      ; backend : string
      ; tags : string String.Map.t
      }
    [@@deriving sexp]
  end

  type t = V1 of V1.t [@@deriving sexp]

  let to_filename t = t |> sexp_of_t |> Sexp.to_string_mach |> Uri.pct_encode
  let of_filename_exn s = s |> Uri.pct_decode |> Sexp.of_string |> t_of_sexp
end

module Private = struct
  let machine_output_mode_envvar = "BONSAI_BENCH_MACHINE_OUTPUT_MODE"
  let machine_output_tempdir_envvar = "BONSAI_BENCH_MACHINE_OUTPUT_TEMPDIR"
end
