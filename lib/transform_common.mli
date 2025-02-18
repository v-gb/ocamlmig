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
      Ocamlformat_lib.Parse_with_comments.with_comments
    * Ocamlformat_lib.Conf_t.t)
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

module Requalify : sig
  val same_resolution :
       ('a * _) Uast.ns
    -> Fmast.Longident.t * Uast.env
    -> Fmast.Longident.t * Uast.env
    -> [> `No of 'a | `Unknown | `Yes ]

  val ident_of_path_exn : Uast.Path.t -> Fmast.Longident.t
  val idents_of_path : Uast.Path.t -> Fmast.Longident.t list

  val requalify :
       (Uast.Path.t * 'a) Uast.ns
    -> Uast.env
    -> Uast.env
    -> Fmast.Longident.t
    -> Fmast.Longident.t
  (** If the given identifier has a different meaning in env1 vs env2 (meaning different
      shape uids), provide a different identifier than should have the same meaning in
      env2 as the initial identifier has in env1. Concretely, the identifier might be
      "Int.to_string", env1 might be somewhere in Ocamlmig.stdlib_to_stdlib, and env2
      would be a call site of Int.to_string. If Int.to_string in env2 doesn't point to the
      Int.to_string from the stdlib (because Int has been shadowed, say), then we'd return
      Stdlib.Int.to_string. *)

  val try_unqualifying_ident :
       same_resolution_as_initially:(Fmast.Longident.t -> bool)
    -> Uast.Env_summary.t
    -> Fmast.Longident.t
    -> Fmast.Longident.t
  (** Given an identifier, like Stdlib.Int.to_string, try to provide a shorter version of
      that identifier that points to the same value. Concretely, this consults any open in
      the environment, and tries to chop any opened module from the identifier. We could
      also consult aliases like [module H = Hashtbl], but we don't. *)
end
