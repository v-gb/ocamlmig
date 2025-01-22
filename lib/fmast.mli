(** "fmast" stands for "ocamlformat ast", meaning the version of the ocaml AST that
   ocamlformat parses/print.

   The parsing/printing is done with ocamlformat, because we want to express
   modifications of the source file by modifying the AST, rather than by tweaking bytes
   in the source code strings. As explained in Dyn_ocamlformat, this module doesn't
   have to print files in the same exact format used by users, we just want to round
   trip the code through the AST while avoiding undesirable normalisations of the source
   code, like dropping comments, turning [let x : foo = y] into [let x = (y : foo)] or
   vice versa, etc.

   Note: ocamlformat makes some assumptions of the form "the AST we print is the
   AST we parsed", which we break and have to either workaround or patch.
 *)

open Base
module Format := Stdlib.Format
module Parsetree = Ocamlformat_parser_extended.Parsetree

val ocamlformat_print :
     'ext Ocamlformat_lib.Extended_ast.t
  -> conf:Ocamlformat_lib.Conf_t.t
  -> 'ext Ocamlformat_lib.Parse_with_comments.with_comments
  -> string

val debug_print :
     ?raw:bool
  -> 'a Ocamlformat_lib.Extended_ast.t
  -> ?fmconf:Ocamlformat_lib.Conf_t.t
  -> 'a
  -> string

val debug_print_pattern :
     ?fmconf:Ocamlformat_lib.Conf_t.t
  -> Ocamlformat_parser_extended.Parsetree.pattern
  -> string

val parse_with_ocamlformat :
     'a Ocamlformat_lib.Extended_ast.t
  -> conf:Ocamlformat_lib.Conf_t.t
  -> input_name:string
  -> string
  -> 'a Ocamlformat_lib.Parse_with_comments.with_comments

val update_structure :
     'a Ocamlformat_lib.Parse_with_comments.with_comments
  -> ('a -> ('b * 'c) option)
  -> ('b Ocamlformat_lib.Parse_with_comments.with_comments * 'c) option

open! Ocamlformat_ocaml_common
open! Ocamlformat_parser_extended

module Location : sig
  include module type of struct
    include Location
  end

  include sig
    type position = Lexing.position [@@deriving compare, hash, sexp_of]
  end

  include sig
      type nonrec t [@@deriving compare, hash, sexp_of]
    end
    with type t := t

  include sig
      type 'a loc [@@deriving compare, sexp_of]
    end
    with type 'a loc := 'a loc
end

module Longident : sig
  include module type of struct
    include Longident
  end

  include sig
      type nonrec t [@@deriving compare, sexp_of]
    end
    with type t := t
end

module Ast_helper : sig
  module P := Parsetree

  include module type of struct
      include Ast_helper
    end
    with module Pat := Ast_helper.Pat
     and module Exp := Ast_helper.Exp

  val located : ?loc:loc -> 'a -> 'a with_loc

  module Pat : sig
    include module type of struct
      include Ast_helper.Pat
    end

    val some : ?loc:loc -> P.pattern -> P.pattern
    val none : ?loc:loc -> unit -> P.pattern
  end

  module Exp : sig
    include module type of struct
      include Ast_helper.Exp
    end

    val ident' : ?loc:loc -> Longident.t -> P.expression

    val let' :
      ?rec_:bool -> P.value_binding list -> loc_in:loc -> P.expression -> P.expression

    val some : ?loc:loc -> P.expression -> P.expression
    val none : ?loc:loc -> unit -> P.expression
    val true_ : ?loc:loc -> unit -> P.expression
    val false_ : ?loc:loc -> unit -> P.expression
    val unit : ?loc:loc -> unit -> P.expression
  end
end

type function_arg = Asttypes.arg_label * Parsetree.expression

val pp_function_arg :
  ?raw:bool -> Format.formatter -> Asttypes.arg_label * Parsetree.expression -> unit

val pp_function_args :
     ?raw:bool
  -> Format.formatter
  -> (Asttypes.arg_label * Parsetree.expression) list
  -> unit

type expression = Parsetree.expression

val sexp_of_expression : ?raw:bool -> expression -> Sexp.t

type pattern = Parsetree.pattern [@@deriving sexp_of]
type structure = Parsetree.structure [@@deriving sexp_of]
type structure_item = Parsetree.structure_item [@@deriving sexp_of]
type arg_label = Asttypes.arg_label [@@deriving sexp_of]

val type_constraint_of_value_constraint :
  Parsetree.value_constraint -> Parsetree.type_constraint

val value_constraint_of_type_constraint :
  Parsetree.type_constraint -> Parsetree.value_constraint
