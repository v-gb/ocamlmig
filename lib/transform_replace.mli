(** A module that implements a form of AST to AST rewriting, without writing code
    directly, at the command line. Like sed, but structured. There is much to improve
    here. *)

open Base
open Common

val run :
     listing:Build.Listing.t
  -> (string * string) list
  -> unit
  -> fmconf:Ocamlformat_lib.Conf_t.t
  -> type_index:Build.Type_index.t option Lazy.t
  -> source_path:Cwdpath.t
  -> input_name_matching_compilation_command:string option Lazy.t
  -> Transform_common.result option
