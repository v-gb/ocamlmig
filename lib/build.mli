(** Modules to locate the dune _build directory, build artifacts in it, and provide
    information from these cmts to the rewriting code. *)

open Base
open Common

val read_cmt : Cwdpath.t -> Cmt_format.cmt_infos
val comp_unit_of_uid : Shape.Uid.t -> string option
val input_name_matching_compilation_command : Cmt_format.cmt_infos -> string option
val is_dune_root : string -> bool

module Listing : sig
  (** Metadata, usually extracted from dune, about the compilation of ml files: where the
      artifacts live, what search path is given to the compiler when building them. *)

  type one =
    { root : Abspath.t
    ; source : Cwdpath.t
    ; compilation_unit : string
    ; cmt_dirs : Cwdpath.t list
    ; cmt_load_paths : Load_path.Dir.t Lazy.t list
    }
  [@@deriving sexp_of]

  type t =
    { by_source : one Map.M(Cwdpath).t
    ; by_compilation_unit : one list Map.M(String).t
    ; all_load_paths : Load_path.Dir.t Lazy.t Hashtbl.M(Cwdpath).t
    }
  [@@deriving sexp_of]

  val create : dune_root:Abspath.t -> source_paths:Cwdpath.t list -> t
  val create_without_dune : Abspath.t -> t
  val locate_cmt : t -> source_path:Cwdpath.t -> (Cwdpath.t * one, Sexp.t) Result.t
end

module Artifacts : sig
  (** The typing interfaces are confusing.

      .cmt files contain: 1. typed trees 2. an optional mapping from uid to decl 3. an
      optional mapping from lid+loc to shape 4. the shape of the implementation

      1. The typed trees contain Env.t values, but they can't be used as is. One must call
      Envaux.env_of_only_summary first, otherwise you're using what is effectively empty
      environments. The environment contains shape queriable via Env.shape_of_path, but
      these shapes are not saved in the .cmt, and I think you end up with an approximation
      when you query it (always returns Leaf, for modules, so approximated leaves for
      values). The typed tree also contains these Shape.Uid.t on every use of an
      identifier and definition of a variable, in a matching way. But (surprise!), these
      identifiers are not globally unique: [Item { comp_unit; id }] might be referring to
      either the "val" in the .mli, or the "let" in the .ml, presumably depending on
      whether the .mli exists and depending on if you're referring to the value from the
      inside of the compilation unit or from the outside. If you have the "val", I am not
      sure there's any way to link that back to the "let" implementing it.

      2. The mapping from uids of declarations in a cmt to the declaration themselves has
      several problems:
      - dune builds it, but the stdlib doesn't, so it's not always present. It seems to be
        computed by processing of the cmt, in which case it should be buildable on demand
        when empty.
      - AFAIU, it doesn't contain the implied bindings created by coercing the .ml to the
        type of the the .mli.

      3. 4. these seem fine, except that the shapes can be approximated, which is harmful
      for us, and there's nothing we can do about it, because the compiler has already
      lost the information. Separately, Shape_reduce says not to reduce Make functor
      across compilation units, which we try to do, although it's not clear that this
      warning makes any senes, considering a single call to reduce will reduce across
      compilation unit boundaries. *)

  type t [@@deriving sexp_of]
  type cache [@@deriving sexp_of]

  val create_cache : unit -> cache

  type find_decl_result =
    (Shape.Uid.t * Typedtree.item_declaration option, string Lazy.t) Result.t

  val create : ?cache:cache -> Listing.t -> t

  val sigitem_from_def_uid :
       t
    -> Uast.Shape.Uid.t * [ `impl | `intf ]
    -> [ `Decl of Typedtree.item_declaration | `Sigitem of Types.signature_item ] option

  val decl_from_following_shape : t -> string * Shape.t -> find_decl_result

  val shape_from_occurrence :
    t -> string * Uast.Longident_loc_ignoring_filename.t -> find_decl_result option

  val locate_cmt_from_library_name :
       t
    -> dune_root:Abspath.t
    -> library_name:string
    -> (Cwdpath.t * Cmt_format.cmt_infos) option
end

module Type_index : sig
  (** A typed tree that's been indexed by AST node locations. This is used to compute
      types while working on the parsetree (in fact, the ocamlformat parsetree usually,
      not even the compiler one). *)

  type any_pattern = T : _ Typedtree.general_pattern -> any_pattern
  type t [@@deriving sexp_of]

  val create : Cwdpath.t -> Listing.one -> t
  val create_from_cmt_infos : Cmt_format.cmt_infos -> Listing.one -> t

  (* Not sure in what way setting up the load path is necessary. I guess we'll see
     if this causes problems or not. Maybe instead of trying to mimic the load paths
     of the program, I should just use the maximum load path possible. *)
  val create_without_setting_up_loadpath : Cmt_format.cmt_infos -> t
  val expr : t -> Location.t -> Typedtree.expression list
  val typ : t -> Location.t -> Typedtree.core_type list
  val pat : t -> Location.t -> any_pattern list
  val ce : t -> Location.t -> Typedtree.class_expr list
  val cty : t -> Location.t -> Typedtree.class_type list
  val constr : t -> Location.t -> Types.constructor_description list
end
