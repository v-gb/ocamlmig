(** "fmast" stands for "ocamlformat ast", meaning the version of the ocaml AST that
    ocamlformat parses/print.

    The parsing/printing is done with ocamlformat, because we want to express
    modifications of the source file by modifying the AST, rather than by tweaking bytes
    in the source code strings. As explained in Dyn_ocamlformat, this module doesn't have
    to print files in the same exact format used by users, we just want to round trip the
    code through the AST while avoiding undesirable normalisations of the source code,
    like dropping comments, turning [let x : foo = y] into [let x = (y : foo)] or vice
    versa, etc.

    Note: ocamlformat makes some assumptions of the form "the AST we print is the AST we
    parsed", which we break and have to either workaround or patch. *)

open Base
module Format := Stdlib.Format
module Parsetree = Ocamlformat_parser_extended.Parsetree

val ocaml_version : Ocamlformat_lib.Conf.t -> Ocaml_version.t
val ocaml_version' : Ocamlformat_lib.Conf.t -> int * int

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

  module Ignoring_filename : sig
    type position = Lexing.position [@@deriving compare, hash, sexp_of]
    type nonrec t = t [@@deriving compare, hash, sexp_of]
    type nonrec 'a loc = 'a loc [@@deriving compare, sexp_of]
  end
end

module Longident : sig
  include module type of struct
    include Longident
  end

  include sig
      type nonrec t [@@deriving compare, sexp_of]
    end
    with type t := t

  include Comparator.S with type t := t

  val map_modpath : t -> (t Location.loc -> t Location.loc) -> t
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
    val ext_exp : ?loc:loc -> string -> P.expression -> P.pattern

    val unit :
         ?loc:loc
      -> ?attrs:Ocamlformat_parser_extended.Parsetree.attributes
      -> unit
      -> P.pattern

    val unlabelled_tuple : ?loc:loc -> ?attrs:P.attributes -> P.pattern list -> P.pattern
  end

  module Exp : sig
    include module type of struct
      include Ast_helper.Exp
    end

    val ident' : ?loc:loc -> Longident.t -> P.expression

    val let' :
         ?loc:loc
      -> ?rec_:bool
      -> P.value_binding list
      -> loc_in:loc
      -> P.expression
      -> P.expression

    val some : ?loc:loc -> P.expression -> P.expression
    val none : ?loc:loc -> unit -> P.expression
    val true_ : ?loc:loc -> unit -> P.expression
    val false_ : ?loc:loc -> unit -> P.expression

    val unit :
         ?loc:loc
      -> ?attrs:Ocamlformat_parser_extended.Parsetree.attributes
      -> unit
      -> P.expression

    val string : ?loc:loc -> ?quotation_delimiter:string -> string -> P.expression
    val int : ?loc:loc -> ?suffix:char -> int -> P.expression

    val ext_exp :
      ?loc:loc -> ?attrs:P.attributes -> string -> P.expression -> P.expression

    val unlabelled_tuple :
      ?loc:loc -> ?attrs:P.attributes -> P.expression list -> P.expression
  end

  module Attr : sig
    include module type of struct
      include Ast_helper.Attr
    end

    val exp : ?loc:loc -> string -> P.expression -> P.attribute
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
type core_type = Parsetree.core_type [@@deriving sexp_of]
type class_field = Parsetree.class_field [@@deriving sexp_of]
type class_type = Parsetree.class_type [@@deriving sexp_of]
type module_expr = Parsetree.module_expr [@@deriving sexp_of]
type module_type = Parsetree.module_type [@@deriving sexp_of]
type signature_item = Parsetree.signature_item [@@deriving sexp_of]
type signature = Parsetree.signature [@@deriving sexp_of]

val type_constraint_of_value_constraint :
  Parsetree.value_constraint -> Parsetree.type_constraint

val value_constraint_of_type_constraint :
  Parsetree.type_constraint -> Parsetree.value_constraint

type arg_label = Asttypes.arg_label [@@deriving sexp_of]

module Arg_label : sig
  type t = arg_label [@@deriving equal, sexp_of]

  val to_string : t -> string
end

module Node : sig
  type ('w, 'node, 'desc, 'env) t =
    | Exp : ([> `Exp ], expression, Parsetree.expression_desc, Typedtree.expression) t
    | Pat : ([> `Pat ], pattern, Parsetree.pattern_desc, Uast.any_pattern) t
    | Typ : ([> `Typ ], core_type, Parsetree.core_type_desc, Typedtree.core_type) t
    | Mexp :
        ([> `Mexp ], module_expr, Parsetree.module_expr_desc, Typedtree.module_expr) t
    | Mtyp :
        ([> `Mtyp ], module_type, Parsetree.module_type_desc, Typedtree.module_type) t
    | Cexp :
        ( [> `Cexp ]
        , Parsetree.class_expr
        , Parsetree.class_expr_desc
        , Typedtree.class_expr )
        t
    | Ctyp : ([> `Ctyp ], class_type, Parsetree.class_type_desc, Typedtree.class_type) t
    | Value_binding : ([> `Value_binding ], Parsetree.value_binding, unit, unit) t
    | Binding_op : ([> `Binding_op ], Parsetree.binding_op, unit, unit) t

  type desc =
    [ `Exp
    | `Pat
    | `Typ
    | `Mexp
    | `Mtyp
    | `Cexp
    | `Ctyp
    ]

  val loc : (_, 'a, _, _) t -> 'a -> Location.t
  val attributes : (_, 'a, _, _) t -> 'a -> Parsetree.attributes
  val desc : (desc, 'a, 'desc, _) t -> 'a -> 'desc

  val update :
    ?desc:'desc -> ?attributes:Parsetree.attributes -> (desc, 'a, 'desc, _) t -> 'a -> 'a

  val meth :
    (_, 'node, _, _) t -> Ast_mapper.mapper -> Ast_mapper.mapper -> 'node -> 'node

  val map : (_, 'node, _, _) t -> Ast_mapper.mapper -> 'node -> 'node
end

module Flat_longident : sig
  type t = string Location.loc * cont Location.loc list

  and cont =
    | Dot of string Location.loc
    | Apply_to of t
  [@@deriving equal, compare, hash, sexp_of]

  val from_longident : Longident.t Location.loc -> t
  val to_longident : t -> Longident.t Location.loc
  val is_prefix : t -> prefix:t -> bool
  val chop_prefix : t -> prefix:t -> t option
end
