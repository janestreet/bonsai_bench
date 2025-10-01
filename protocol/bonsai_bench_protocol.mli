open! Core

module Dimensions : sig
  (** A [Dimensions.t] represents the "parameters" to some benchmark result.
      [benchmark_name] and [comparison_config_name] exist across all benchmarks. [tags] is
      intended for benchmark-specific things, e.g. various numerical parameters. *)

  module V1 : sig
    type t =
      { benchmark_name : string
      ; comparison_config_name : string option
      ; backend : string
      ; tags : string String.Map.t
      }
    [@@deriving sexp_of]
  end

  type t = V1 of V1.t [@@deriving sexp]

  val to_filename : t -> string
  val of_filename_exn : string -> t
end

module Machine_output : sig
  module V1 : sig
    module Measurement : sig
      type t =
        { dimensions : Dimensions.V1.t
        ; samples : Core_bench_internals.Measurement_sample.t list
        }
      [@@deriving sexp_of]
    end

    type t = { measurements : Measurement.t list } [@@deriving sexp]
  end

  (* Only the stabilized type exposes [of_sexp]. *)
  type t = V1 of V1.t [@@deriving sexp]
end
