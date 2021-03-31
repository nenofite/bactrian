open! Core_kernel

include module type of Types

val main : unit -> string

type state

val setup : (string * string list) list -> state

val test : state -> (string * string) list -> unit

val finish : state -> (unit, string) result

val alcotest : state -> unit -> unit
