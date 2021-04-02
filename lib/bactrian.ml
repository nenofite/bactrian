open! Core_kernel
include Types

let main () =
  let () = Logs.debug (fun m -> m "Program has started") in
  "Hello, World!"

type state = {
  factors : factor list;
  mutable test_cases : test_case list;
  mutable fails : string list;
}

let setup factors =
  let factors =
    List.mapi factors ~f:(fun rank (factor, values) ->
        make_factor factor ~rank
          ~possible_values:
            (List.mapi values ~f:(fun rank value -> make_value value ~rank)))
  in
  { factors; test_cases = []; fails = [] }

let test' s tc =
  let tc, fails =
    List.map tc ~f:(fun (factor, value) ->
        let ( let* ) = Result.( >>= ) in
        let* factor = find_factor factor ~from:s.factors in
        let* value = find_value value ~from:factor in
        Result.return (factor, value))
    |> List.partition_map ~f:Result.to_either
  in
  s.test_cases <- tc :: s.test_cases;
  s.fails <- fails @ s.fails;
  tc

let test s tc = test' s tc |> ignore

let named_test s tc = test' s tc |> Show.test_case

let finish s =
  let { factors; test_cases; fails } = s in
  match fails with
  | [] -> Check.check ~factors ~test_cases
  | fails -> Result.fail (String.concat ~sep:"\n" fails)

let alcotest s () =
  Alcotest.(check (result unit string))
    "No missing tests" (Result.return ()) (finish s)
