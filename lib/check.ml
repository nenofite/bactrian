open! Core_kernel
(* open Types *)

(* let value_exists value ~factor ~test_cases =
  List.exists test_cases ~f:(fun tc ->
      List.exists tc ~f:(Poly.equal (factor, value))) *)

let check ~factors ~test_cases =
  let fixed_test_cases =
    Pairwise.sort_and_ensure_all_pairs factors ~table:test_cases
  in
  let missing =
    List.filter fixed_test_cases ~f:(fun fixed_tc ->
        not (List.exists test_cases ~f:(Poly.equal fixed_tc)))
  in
  (* let missing =
       List.concat_map factors ~f:(fun (factor, values) ->
           List.filter_map values ~f:(fun value ->
               if value_exists value ~factor ~test_cases then None
               else Some (factor, value)))
     in *)
  match missing with
  | [] -> Result.return ()
  | missing -> Result.fail (Show.missing missing)
