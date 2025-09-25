(** The core of ocamlmig: given a .ml, and type/scoping information, apply [@migrate]
    attributes to the AST, and returns the resulting .ml. *)

open Base
open Common

type res =
  { libraries : string list
  ; pps : string list
  }

val run :
     artifacts:string * Build.Artifacts.t
  -> type_index:Build.Type_index.t
  -> extra_migrations_cmts:(Cwdpath.t * Cmt_format.cmt_infos) option
  -> fmconf:Ocamlformat_lib.Conf_t.t
  -> source_path:Cwdpath.t
  -> module_migrations:bool
  -> input_name_matching_compilation_command:string option
  -> (Transform_common.result * res) option

(**/**)

module Ast_mapper := Ocamlformat_parser_extended.Ast_mapper
module P := Fmast.Parsetree

type repl =
  { loc_preserved : Fmast.expression
  ; loc_updated : Fmast.expression
  }

type 'repl gen_migrate_payload =
  { repl : 'repl
  ; libraries : string list
  ; pps : string list
  }

type migrate_payload = repl gen_migrate_payload [@@deriving sexp_of]

val find_extra_migration_fmast :
     P.expression
  -> (P.expression
     * Fmast.Longident.t Fmast.Location.loc
     * [ `Id | `Structure ]
     * migrate_payload)
     option

val find_attribute_payload_fmast :
  ?repl:P.expression -> P.attributes -> migrate_payload option

val update_attribute_payload_fmast :
  P.attributes -> P.expression option gen_migrate_payload -> P.attributes

val remove_attribute_payload_fmast : P.attributes -> P.attributes
val has_context_match : P.expression -> bool
val commute_args : Fmast.function_arg list -> Fmast.function_arg list

val commute_list :
  ('a -> 'a -> bool) -> ('a * P.expression) list -> ('a * P.expression) list

val internalize_attribute_mapper : Ast_mapper.mapper
