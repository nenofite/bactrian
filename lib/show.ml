open! Core_kernel
open Types

let factor_value_set set =
  sort_test_case set
  |> List.map ~f:(fun (f, v) -> show_factor_value f v)
  |> String.concat ~sep:"; "

let missing missing =
  let ls =
    List.rev_map missing ~f:(fun tc ->
        Printf.sprintf "* %s" (factor_value_set tc))
    |> String.concat ~sep:"\n"
  in
  Printf.sprintf "Missing tests:\n%s" ls

let test_case tc = factor_value_set tc

let unknown_factors factors =
  List.map factors ~f:factor_name |> String.concat ~sep:"; "
