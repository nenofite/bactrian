open! Core_kernel

type factor = Factor of string

type value = Value of string

type test_case = (factor, value) List.Assoc.t

type factor_def = factor * value list

type table = test_case list

let pair_exists a b ~table =
  let tc_has_pair tc =
    List.exists tc ~f:(Poly.equal a) && List.exists tc ~f:(Poly.equal b)
  in
  List.exists table ~f:tc_has_pair

let ensure_pair ((fac_a, val_a) as a) ((fac_b, val_b) as b) ~table =
  if pair_exists a b ~table then table else add_pair table
