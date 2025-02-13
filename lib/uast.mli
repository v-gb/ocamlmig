(** "uast" stands for "upstream ast", meaning the ast of the ocaml compiler, as opposed to
    the ocamlformat ast.

    This module does two things:
    - hold a few functionality related to the upstream AST, notably the ability to just
      debug print things
    - make it possible to refer to the upstream AST modules when they got shadowed by an
      open Ocamlformat*

    We need to interact with the upstream AST mainly (only?) for the type information and
    scoping information in .cmt files. We do not use the upstream parse/print, and instead
    use the ocamlformat ones, since the parse/AST/print pipeline drops information that's
    useful to print modified source files, and we generally want to print back an AST,
    rather than merely tweaking the string of the source files. *)

open Base
module Format = Stdlib.Format
module Asttypes = Asttypes
module Parsetree = Parsetree
module Parser = Parser
module Parse = Parse
module Pprintast = Pprintast

val typed_print_value_binding : Format.formatter -> Typedtree.value_binding -> unit

val typed_print_value_description :
  Format.formatter -> Typedtree.value_description -> unit

val typed_print_signature_items :
  Format.formatter -> Typedtree.signature_item list -> unit

val typed_print_structure_items :
  Format.formatter -> Typedtree.structure_item list -> unit

module Longident : sig
  include module type of struct
    include Longident
  end

  include sig
      type nonrec t [@@deriving compare, sexp_of]
    end
    with type t := t
end

module Path : sig
  include module type of struct
    include Path
  end

  include sig
      type nonrec t [@@deriving sexp_of]
    end
    with type t := t
end

module Shape : sig
  include module type of struct
      include Shape
    end
    with module Uid := Shape.Uid
    with module Sig_component_kind := Shape.Sig_component_kind

  include sig
      type nonrec t [@@deriving sexp_of]
    end
    with type t := t

  module Sig_component_kind : sig
    include module type of struct
      include Shape.Sig_component_kind
    end

    include sig
        type nonrec t [@@deriving sexp_of]
      end
      with type t := t
  end

  module Item : sig
    include module type of struct
        include Shape.Item
      end
      with module Map := Shape.Item.Map

    include sig
        type nonrec t [@@deriving sexp_of]
      end
      with type t := t

    module Map : sig
      include module type of struct
        include Shape.Item.Map
      end

      include sig
          type nonrec 'a t [@@deriving sexp_of]
        end
        with type 'a t := 'a t
    end
  end

  module Uid : sig
    include module type of struct
      include Shape.Uid
    end

    include sig
        type nonrec t [@@deriving sexp_of]
      end
      with type t := t
  end
end

module Shape_reduce : sig
  include module type of struct
    include Shape_reduce
  end

  include sig
      type nonrec result [@@deriving sexp_of]
    end
    with type result := result
end

module Location : sig
  include module type of struct
    include Location
  end

  type position = Lexing.position [@@deriving sexp_of]

  include sig
      type nonrec t [@@deriving sexp_of]
    end
    with type t := t

  include sig
      type 'a loc [@@deriving sexp_of]
    end
    with type 'a loc := 'a loc

  module Including_filename : sig
    (** Compare and hash, including the filename. This can be lead to problems, because
        the .cmt contains locations created in the context of source repositories (say
        examples/stdlib_to_stdlib/stdlib_to_stdlib.ml), whereas if we read a source file
        like we do for side migrations, then the filename of the locations in that
        parsetree may be "_opam/lib/stdlib_to_stdlib/stdlib_to_stdlib.ml", thus leading to
        lookups in the cmt all failing.

        On the other hand, ignoring the filename may lead to spuriously successful
        lookups. So this only makes sense for maps that are limited to a single file. *)

    type position = Lexing.position [@@deriving compare, hash, sexp_of]
    type nonrec t = t [@@deriving compare, hash, sexp_of]
    type nonrec 'a loc = 'a loc [@@deriving compare, sexp_of]
  end

  module Ignoring_filename : sig
    type position = Lexing.position [@@deriving compare, hash, sexp_of]
    type nonrec t = t [@@deriving compare, hash, sexp_of]
    type nonrec 'a loc = 'a loc [@@deriving compare, sexp_of]
  end
end

module Longident_loc_ignoring_filename : sig
  type t = Longident.t Location.loc [@@deriving compare, sexp_of]

  include Comparator.S with type t := t
end

module Env_summary : sig
  type t = Env.summary [@@deriving sexp_of]

  val next : t -> t option
  val next_exn : t -> t
  val set_exn : t -> next:t -> t
  val length : t -> int
  val at_exn : t -> int -> (t -> t) -> t
end

type env = Env.t [@@deriving sexp_of]

val initial_env : Env.t Lazy.t
val type_type : Parsetree.core_type -> Types.type_expr
val match_typ : env:Env.t -> Types.type_expr -> user_type:Types.type_expr -> bool
val decl_attributes : Typedtree.item_declaration -> Typedtree.attributes
val sigitem_attributes : Types.signature_item -> Parsetree.attributes

val decl_or_sigitem_attributes :
     [ `Decl of Typedtree.item_declaration | `Sigitem of Types.signature_item ]
  -> Parsetree.attributes

type 'a ns =
  | Value : (Path.t * Types.value_description) ns
  | Type : (Path.t * Types.type_declaration) ns
  | Module : (Path.t * Types.module_declaration) ns
  | Module_type : (Path.t * Types.modtype_declaration) ns
  | Class : (Path.t * Types.class_declaration) ns
  | Class_type : (Path.t * Types.class_type_declaration) ns
  | Constructor : Types.constructor_description ns
  | Label : Types.label_description ns

val find_by_name : 'a ns -> env -> Longident.t -> 'a
val uid : (Path.t * 'a) ns -> 'a -> Shape.Uid.t
