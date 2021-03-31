open! Core_kernel
open Types

let value_exists value ~factor ~test_cases =
  List.exists test_cases ~f:(fun tc ->
      List.exists tc ~f:(Poly.equal (factor, value))
      (* ~f:(fun (here_factor, here_value) ->
          String.(here_factor = factor && here_value = value)) *))

let show_missing missing =
  let ls =
    List.map missing ~f:(fun (Factor factor, Value value) ->
        Printf.sprintf "%s=%s" factor value)
    |> String.concat ~sep:" "
  in
  Printf.sprintf "Missing tests: %s" ls

let check ~factors ~test_cases =
  let missing =
    List.concat_map factors ~f:(fun (factor, values) ->
        List.filter_map values ~f:(fun value ->
            if value_exists value ~factor ~test_cases then None
            else Some (factor, value)))
  in
  match missing with
  | [] -> Result.return ()
  | missing -> Result.fail (show_missing missing)

(* let pair_exists a b ~table =
  let tc_has_pair tc =
    List.exists tc ~f:(Poly.equal a) && List.exists tc ~f:(Poly.equal b)
  in
  List.exists table ~f:tc_has_pair *)

(* let ensure_pair ((fac_a, val_a) as a) ((fac_b, val_b) as b) ~table =
  if pair_exists a b ~table then table else add_pair table *)
