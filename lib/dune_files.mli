(** Functionality to access and edit dune configuration files like **/dune and
    dune-project. We edit files in particular in case a migration creates new dependencies
    on the client code, for instance if they moved some functionality into a separate
    library. *)

open! Base
open! Stdio
open! Common

val ppx : path:Cwdpath.t -> string list

val add_dependencies :
     dune_root:Abspath.t
  -> ([ `Path of Cwdpath.t ] * string list * string list) list
  -> ([ `Path of Cwdpath.t ] * string * string) list
