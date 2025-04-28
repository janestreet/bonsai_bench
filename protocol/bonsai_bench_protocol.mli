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
    [@@deriving sexp]
  end

  type t = V1 of V1.t [@@deriving sexp]

  val to_filename : t -> string
  val of_filename_exn : string -> t
end

module Private : sig
  val machine_output_mode_envvar : string
  val machine_output_tempdir_envvar : string
end
