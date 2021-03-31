open! Core_kernel
open Types

let factor_value_set set =
  List.map set ~f:(fun (Factor factor, Value value) ->
      Printf.sprintf "%s=%s" factor value)
  |> String.concat ~sep:"; "

let missing missing =
  Printf.sprintf "Missing tests: %s" (factor_value_set missing)

let test_case tc = factor_value_set tc
