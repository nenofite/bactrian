open! Core_kernel

type value = { name : string; rank : int }

type factor = { name : string; rank : int; possible_values : value list }

type test_case = (factor, value) List.Assoc.t

type table = test_case list

let make_factor name ~rank ~possible_values = { name; rank; possible_values }

let make_value name ~rank = { name; rank }

let compare_factor (a : factor) (b : factor) = Int.compare a.rank b.rank

let compare_value (a : value) (b : value) = Int.compare a.rank b.rank

let factor_name (factor : factor) = factor.name

let value_name (value : value) = value.name

let find_factor name ~(from : factor list) =
  List.find from ~f:(fun factor -> String.(factor.name = name))
  |> Result.of_option ~error:(Printf.sprintf "Unknown factor: %s" name)

let find_value name ~(from : factor) =
  List.find from.possible_values ~f:(fun value -> String.(value.name = name))
  |> Result.of_option
       ~error:
         (Printf.sprintf "Unknown value %s in factor %s" name (factor_name from))

let show_factor_value factor value =
  Printf.sprintf "%s=%s" (factor_name factor) (value_name value)

let sort_test_case (tc : test_case) =
  List.stable_sort tc ~compare:(fun (factor_a, _) (factor_b, _) ->
      compare_factor factor_a factor_b)
