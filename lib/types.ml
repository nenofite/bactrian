open! Core_kernel

type factor = Factor of string

type value = Value of string

type test_case = (factor, value) List.Assoc.t

type factor_def = factor * value list

type table = test_case list
