(** Functionality to turn migration attributes into attributes that request
    the inverse migration, on a best effort basis.

    For instance:

      let _ = List.map [@@migrate { repl = fun f l -> Base.List.map l ~f }]

    would be transformed into:

      let _ = Base.List.map [@@migrate { repl = fun l ~f -> List.map f l }]
 *)

open! Base
open! Common

val run :
     fmconf:Ocamlformat_lib.Conf_t.t
  -> source_path:Cwdpath.t
  -> input_name_matching_compilation_command:string option
  -> (string * string) option
