(** Implements a few transformations that modify what's in scope, and try to
    update the rest of file to preserve behavior.

    For instance, Unopen { name = "Printf" } would remove all open Printf
    (where printf refers to the compilation unit) from the scope, and, say,
    update all mentions of printf into Printf.printf.
 *)

open Base
open Common

type t =
  | Unopen of
      { name : string
      ; conservative : bool
      }
  | Open of
      { name : string
      ; bang : bool
      ; conservative : bool
      }
  | Unqualify of string list

val run :
     fmconf:Ocamlformat_lib.Conf_t.t
  -> artifacts:Build.Artifacts.t
  -> type_index:Build.Type_index.t
  -> cmt_infos:Cmt_format.cmt_infos
  -> t
  -> source_path:Cwdpath.t
  -> input_name_matching_compilation_command:string option
  -> Transform_common.result option

(**/**)

val ident_of_path : Uast.Path.t -> Fmast.Longident.t
