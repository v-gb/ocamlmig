(** An experiment at building a transformation without requiring ocamlformat or dune, here
    taking making code compatible with -strict-sequence as an arbitrary test case. It
    works fine, although probably not for more involved changes. *)

open! Base
open Common

val run : type_index:Build.Type_index.t -> Cwdpath.t -> (string * string) option
