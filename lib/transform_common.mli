(** Basic utility functions that many transforms would need. *)

open! Base
open! Common

(** We try to use this filename on every node we insert into the tree, so we can
    distinguish our our nodes. *)

val migrate_filename_import : string
val migrate_filename_gen : string
val migrate_filename : [< `Gen | `Import ] -> string

module P := Fmast.Parsetree
module Ast_mapper := Ocamlformat_parser_extended.Ast_mapper

val is_migrate_filename : Ocamlformat_ocaml_common.Location.t -> bool
val migrate_loc : [< `Gen | `Import ] -> Ocamlformat_ocaml_common.Location.t

val with_log :
  (Ast_mapper.mapper -> P.expression -> 'a) -> Ast_mapper.mapper -> P.expression -> 'a

module Attr : sig
  type t = P.attributes

  val find : t -> string -> P.attribute option
  val exists : t -> string -> bool
  val prefix : string
  val reorder : [< `Internal | `Source ] -> string

  val create :
       loc:Ocamlformat_ocaml_common.Location.t
    -> string
    -> P.expression option
    -> P.attribute
end

module Sattr : sig
  type 'a t =
    { name : string
    ; build : loc:Ocamlformat_ocaml_common.Location.t -> 'a -> P.attribute
    ; match_ : P.attribute -> 'a
    }

  val find : 'a t -> P.attributes -> 'a option
  val exists : 'a t -> P.attributes -> bool
  val match_expr : (P.expression -> 'a) -> P.attribute -> 'a
  val id : int t
  val pref : P.expression t
  val pun : unit t
  val touched : unit t
  val orig : P.expression t
end

val update_migrate_test_payload :
     ?match_attr:(string -> bool)
  -> ?state:bool ref
  -> changed_something:bool ref
  -> Ast_mapper.mapper
  -> Ast_mapper.mapper
  -> P.structure_item
  -> P.structure_item

val drop_concrete_syntax_constructs :
  (Ast_mapper.mapper -> Ast_mapper.mapper -> 'v -> 'v) -> 'v -> 'v

val remove_attributes : Ast_mapper.mapper

val update_loc :
  Fmast.Location.t -> (Lexing.position -> Lexing.position) -> Fmast.Location.t

val preserve_loc_to_preserve_comment_pos :
     (Ast_mapper.mapper -> Ast_mapper.mapper -> 'ast -> 'ast)
  -> from:Fmast.Location.t
  -> 'ast
  -> 'ast

val preserve_loc_to_preserve_comment_pos_expr :
  from:P.expression -> P.expression -> P.expression

val call : Ast_mapper.mapper -> P.structure -> P.structure

val process_ast :
     P.structure
  -> (bool ref -> P.structure -> P.structure * 'other)
  -> (P.structure * 'other) option

type result =
  string
  * string
  * (Ocamlformat_lib.Extended_ast.structure
     Ocamlformat_lib.Parse_with_comments.with_comments
    * Ocamlformat_lib.Extended_ast.structure
      Ocamlformat_lib.Parse_with_comments.with_comments)
    option

val process_file' :
     fmconf:Ocamlformat_lib.Conf_t.t
  -> source_path:Cwdpath.t
  -> input_name_matching_compilation_command:string option
  -> (bool ref -> P.structure -> P.structure * 'other)
  -> (result * 'other) option

val process_file :
     fmconf:Ocamlformat_lib.Conf_t.t
  -> source_path:Cwdpath.t
  -> input_name_matching_compilation_command:string option
  -> (bool ref -> P.structure -> P.structure)
  -> result option
