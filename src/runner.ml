open! Core
open Bonsai
module Interaction = Bonsai_perf_shared.Interaction

type t =
  | T :
      { driver : 'r Bonsai_driver.t
      ; time_source : Bonsai.Time_source.t
      ; inject_action : 'a -> unit Effect.t
      ; interactions : 'a Interaction.Finalized.t array
      }
      -> t

type wrap_create = { f : 'a. (unit -> 'a) -> 'a } [@@unboxed]

(* We perform two optimizations in this step: flattening the interactions and deduping
   stabilizations. Flattening the structure ensures that there's no additional
   overhead to nesting lots of [Many]s when creating benchmarks. Consecutive
   [Stabilize]s don't add anything to benchmarks and would add a function call of
   overhead. *)
let initialize
  ~filter_profiles
  ~wrap_driver_creation
  ~time_source
  ~component
  ~get_inject
  ~interaction
  =
  let driver =
    wrap_driver_creation.f (fun () -> Bonsai_driver.create ~time_source component)
  in
  let inject_action action =
    (* Calling Driver.result every time that inject_action is called
       is important because the value can change during stabilization *)
    let result = Bonsai_driver.result driver in
    (get_inject result) action
  in
  let interactions =
    Interaction.many
      [ Interaction.stabilize
      ; interaction
      ; Interaction.stabilize
      ; Interaction.profile ~name:"end of run"
      ]
    |> Interaction.finalize ~filter_profiles
    |> Array.of_list
  in
  T { driver; time_source; inject_action; interactions }
;;

let run_interactions
  (T { driver; time_source; inject_action; interactions })
  ~handle_profile
  =
  Array.iter
    interactions
    ~f:(Interaction.Finalized.handle ~driver ~time_source ~inject_action ~handle_profile)
;;

let invalidate_observers (T t) = Bonsai_driver.Expert.invalidate_observers t.driver
