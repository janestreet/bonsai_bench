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

(* We version these types because we might want to build benchmarks for older revisions. *)
module Machine_output = struct
  module V1 = struct
    module Measurement = struct
      type t =
        { dimensions : Dimensions.V1.t
        ; samples : Core_bench_internals.Measurement_sample.t list
        }
      [@@deriving sexp]
    end

    type t = { measurements : Measurement.t list } [@@deriving sexp]
  end

  type t = V1 of V1.t [@@deriving sexp]
end
