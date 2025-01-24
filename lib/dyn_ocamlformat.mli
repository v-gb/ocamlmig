(** Formatting an ocaml string the way the user expects it to. Notably, this is different
    from the printing functions in Fmast, because the ocamlformat version in Fmast is
    statically linked into ocamlmig, and thus might make different decisions than the
    ocamlformat version used by the user.

    We could avoid this by requiring that ocamlmig be built at the same version as
    ocamlformat, but that seems like it would cause a lot of friction. *)

open Base
open! Stdio
open! Common

val format : path:Cwdpath.t -> contents:string -> string
(** ocamlformat the given text, using the configuration for the given file path. If
    ocamlformat is disabled for a given file, this appears to style it regardless, which
    is what we want. *)
