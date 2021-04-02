open! Core_kernel
(* open Types *)

let fix_and_check ~factors ~test_cases =
  let fixed_test_cases =
    Pairwise.sort_and_ensure_all_pairs factors ~table:test_cases
  in
  let missing =
    List.filter fixed_test_cases ~f:(fun fixed_tc ->
        not (List.exists test_cases ~f:(Poly.equal fixed_tc)))
  in
  match missing with
  | [] -> Result.return fixed_test_cases
  | missing -> Result.fail (Show.missing missing)

let check_factors ~factors ~test_cases =
  let known_factors = List.map factors ~f:(fun (fac, _) -> fac) in
  let extra_factors =
    List.concat test_cases
    |> List.map ~f:(fun (fac, _) -> fac)
    |> List.filter ~f:(fun fac ->
           not (List.mem known_factors fac ~equal:Poly.equal))
  in
  match extra_factors with
  | [] -> Result.return ()
  | extra -> Result.failf "Unknown factors: %s" (Show.unknown_factors extra)

(* let check_values ~factors ~test_cases =
  let extra_factors =
    List.concat test_cases
    |> List.map ~f:(fun (fac, _) -> fac)
    |> List.filter ~f:(fun fac ->
           not (List.mem known_factors fac ~equal:Poly.equal))
  in
  match extra_factors with
  | [] -> Result.return ()
  | extra -> Result.failf "Unknown factors: %s" (Show.unknown_factors extra) *)

let check ~factors ~test_cases =
  let ( let* ) = Result.( >>= ) in
  let* () = check_factors ~factors ~test_cases in
  let* _fixed = fix_and_check ~factors ~test_cases in
  Result.return ()
