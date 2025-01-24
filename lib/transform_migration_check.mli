(** Modify side migration attributes from let _ = id [@migrate { repl; ... }] into
    something like let _ = [ id; repl ], thus allowing the replacement expression to be
    typechecked, and with the same type as id. *)

open! Base
open! Common

val run :
     fmconf:Ocamlformat_lib.Conf_t.t
  -> source_path:Cwdpath.t
  -> input_name_matching_compilation_command:string option
  -> (string * string) option
