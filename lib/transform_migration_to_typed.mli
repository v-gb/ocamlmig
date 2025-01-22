(** Modify migration attributes by turning let _ = id [@migrate { repl; ... }]
    into let _ = [ id; repl ] [@migrate { ... }].

    If transform_replace could express this, maybe this wouldn't need to be
    builtin. *)

open! Base
open! Common

val run :
     fmconf:Ocamlformat_lib.Conf_t.t
  -> source_path:Cwdpath.t
  -> input_name_matching_compilation_command:string option
  -> (string * string) option
