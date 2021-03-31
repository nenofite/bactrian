open! Core_kernel
open Types

let factor_value_set set =
  List.map set ~f:(fun (Factor factor, Value value) ->
      Printf.sprintf "%s=%s" factor value)
  |> String.concat ~sep:"; "

let missing missing =
  let ls =
    List.map missing ~f:(fun tc -> Printf.sprintf "* %s" (factor_value_set tc))
    |> String.concat ~sep:"\n"
  in
  Printf.sprintf "Missing tests:\n%s" ls

let test_case tc = factor_value_set tc
