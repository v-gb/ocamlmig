(** Experimental functionality for printing an ocamlformat ast without formatting the
    code.

    I estimated early 2025 that the proportion of ocamlformat among groups that publish to
    opam is 38%. But while small projects should just migrate, for larger projects, it's
    not necessarily so easy. So while I decidedly do not want to embark on a costly
    implementation of such functionality, maybe something cheap is possible.

    The idea is that we can diff the ast before vs after, and print the new code as the
    old code, where every modified subexpression is the ocamlformat expression from the
    new ast.

    This is helpful when the changes are near the leaves of the tree, which they usually
    are. For instance, renaming identifiers are always of this form. *)

open Base
open! Common
open! Ocamlformat_ocaml_common
open Ocamlformat_parser_extended
module P := Parsetree
open! Fmast

val print_diff :
     [ `Expr of P.expression * P.expression
     | `Pat of P.pattern * P.pattern
     | `Str of Fmast.structure * Fmast.structure
     | `Stri of P.structure_item * P.structure_item
     | `Typ of P.core_type * P.core_type
     | `Cf of P.class_field * P.class_field
     | `Cty of P.class_type * P.class_type
     ]
  -> unit

val minprint :
     debug_diff:bool
  -> source_contents:string
  -> structure:Fmast.structure Ocamlformat_lib.Parse_with_comments.with_comments
  -> structure':Fmast.structure
  -> string
