open! Core_kernel
(* open Types *)

let assoc_find = List.Assoc.find ~equal:Poly.equal

let pair_exists a b ~table =
  let tc_has_pair tc =
    List.exists tc ~f:(Poly.equal a) && List.exists tc ~f:(Poly.equal b)
  in
  List.exists table ~f:tc_has_pair

let rec attach_one_side a b ~table =
  let factor_a, value_a = a and factor_b, value_b = b in
  match table with
  | [] -> None
  | tc :: rest -> (
      match (assoc_find tc factor_a, assoc_find tc factor_b) with
      | Some here_value_a, None when Poly.equal here_value_a value_a ->
          Some ((b :: tc) :: rest)
      | None, Some here_value_b when Poly.equal here_value_b value_b ->
          Some ((a :: tc) :: rest)
      | _, _ -> (
          match attach_one_side a b ~table:rest with
          | Some rest -> Some (tc :: rest)
          | None -> None))

let attach_both_sides a b ~table =
  let factor_a, _value_a = a and factor_b, _value_b = b in
  match table with
  | [] -> None
  | tc :: rest -> (
      match (assoc_find tc factor_a, assoc_find tc factor_b) with
      | None, None -> Some ((a :: b :: tc) :: rest)
      | _, _ -> (
          match attach_one_side a b ~table:rest with
          | Some rest -> Some (tc :: rest)
          | None -> None))

let append_both a b ~table = [ a; b ] :: table

let ensure_pair a b ~table =
  if pair_exists a b ~table then table
  else
    match attach_one_side a b ~table with
    | Some table -> table
    | None -> (
        match attach_both_sides a b ~table with
        | Some table -> table
        | None -> append_both a b ~table)

let sort_factors factor_defs =
  List.stable_sort factor_defs ~compare:(fun a b ->
      let _, a_values = a and _, b_values = b in
      -1 * Int.compare (List.length a_values) (List.length b_values))

let fold_sorted_pairs factor_defs ~init ~f =
  let rec go_inner earlier_factors factor init f =
    let here_factor, here_values = factor in
    match earlier_factors with
    | [] -> init
    | (e_factor, e_values) :: earlier_factors ->
        let init =
          List.cartesian_product e_values here_values
          |> List.fold ~init ~f:(fun init (e_value, here_value) ->
                 f init ((e_factor, e_value), (here_factor, here_value)))
        in
        go_inner earlier_factors factor init f
  in
  let rec go_outer earlier_factors remaining_factors init f =
    match remaining_factors with
    | [] -> init
    | factor :: remaining_factors ->
        let init = go_inner earlier_factors factor init f in
        go_outer (factor :: earlier_factors) remaining_factors init f
  in
  match factor_defs with
  | [] | [ _ ] -> failwith "fold_sorted_pairs needs at least two factors"
  | fst :: remaining_factors -> go_outer [ fst ] remaining_factors init f

let ensure_one a ~table =
  if List.mem table [ a ] ~equal:Poly.equal then table else [ a ] :: table

let ensure_all_onewise factor_defs ~table =
  List.fold factor_defs ~init:table ~f:(fun table (factor, values) ->
      List.fold values ~init:table ~f:(fun table value ->
          ensure_one (factor, value) ~table))

let sort_and_ensure_all_pairs factor_defs ~table =
  let factor_defs = sort_factors factor_defs in
  match factor_defs with
  | [] | [ _ ] -> ensure_all_onewise factor_defs ~table
  | _ ->
      fold_sorted_pairs factor_defs ~init:table ~f:(fun table (a, b) ->
          ensure_pair a b ~table)
