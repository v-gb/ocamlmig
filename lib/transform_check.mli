(** Typechecks extra migration attributes [let _ = id [@migrate { repl; ... }]], as it's
    very easy to type the wrong thing otherwise and only realize when trying a migration.
    It also helps teach people about the names in scope in the replacement expression. *)

open! Base
open! Common

val run :
     fmconf:Ocamlformat_lib.Conf_t.t
  -> source_path:Cwdpath.t
  -> add_to_load_path:(Set.M(String).t -> unit)
  -> type_index:Build.Type_index.t Lazy.t
  -> input_name_matching_compilation_command:string option
  -> Transform_common.result option
