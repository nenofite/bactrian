open! Core_kernel
include Types

let main () =
  let () = Logs.debug (fun m -> m "Program has started") in
  "Hello, World!"

type state = { factors : factor_def list; mutable test_cases : test_case list }

let setup factors =
  let factors =
    List.map factors ~f:(fun (factor, values) ->
        (Factor factor, List.map values ~f:(fun value -> Value value)))
  in
  { factors; test_cases = [] }

let test' s tc =
  let tc =
    List.map tc ~f:(fun (factor, value) -> (Factor factor, Value value))
  in
  s.test_cases <- tc :: s.test_cases;
  tc

let test s tc = test' s tc |> ignore

let named_test s tc = test' s tc |> Show.test_case

let finish s =
  let { factors; test_cases } = s in
  Pairwise.check ~factors ~test_cases

let alcotest s () =
  let str = finish s |> Result.error |> Option.value ~default:"" in
  Alcotest.(check string) "No missing tests" "" str
