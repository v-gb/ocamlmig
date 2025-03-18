open Base
module Filename = Stdlib.Filename
module Sys = Stdlib.Sys
module Printf = Stdlib.Printf
module Format = Stdlib.Format
open Common
module Asttypes = Asttypes
module Parsetree = Parsetree
module Parser = Parser
module Parse = Parse
module Pprintast = Pprintast
module Ast_mapper = Ast_mapper

let typed_print_signature_items ppf l =
  Printtyped.interface ppf
    { sig_items = l; sig_type = Stdlib.Obj.magic 0; sig_final_env = Stdlib.Obj.magic 0 }

let typed_print_value_description ppf vb =
  typed_print_signature_items ppf
    [ { sig_desc = Tsig_value vb
      ; sig_loc = Location.none
      ; sig_env = Stdlib.Obj.magic None
      }
    ]

let typed_print_structure_items ppf l =
  Printtyped.implementation ppf
    { str_items = l; str_type = Stdlib.Obj.magic 0; str_final_env = Stdlib.Obj.magic 0 }

let typed_print_value_binding ppf vb =
  typed_print_structure_items ppf
    [ { str_desc = Tstr_value (Nonrecursive, [ vb ])
      ; str_loc = Location.none
      ; str_env = Stdlib.Obj.magic None
      }
    ]

let _ = typed_print_value_binding

module Longident = struct
  include Longident

  let compare : t -> t -> int = Stdlib.compare
  let sexp_of_t t = [%sexp (String.concat ~sep:"." (flatten t) : string)]

  include (val Comparator.make ~compare ~sexp_of_t)
end

module Path = struct
  include Path

  let sexp_of_t t = sexp_of_string (Format_doc.asprintf "%a" Path.print t)
end

module Ident = struct
  include Ident

  let sexp_of_t t = sexp_of_string (Format_doc.asprintf "%a" Ident.print_with_scope t)
end

module Unit_info = struct
  include Unit_info

  type intf_or_impl = Unit_info.intf_or_impl =
    | Intf
    | Impl
  [@@deriving compare, sexp_of, hash]
end

module Shape = struct
  include Shape

  module Uid = struct
    include Uid

    type t = Uid.t = private
      | Compilation_unit of string
      | Item of
          { comp_unit : string
          ; id : int
          ; from : Unit_info.intf_or_impl
          }
      | Internal
      | Predef of string
    [@@deriving sexp_of]
  end

  module Sig_component_kind = struct
    include Sig_component_kind

    let sexp_of_t t = sexp_of_string (to_string t)
  end

  module Item = struct
    include Item

    type t = string * Sig_component_kind.t [@@deriving sexp_of]

    module Map = struct
      let sexp_of_t sexp_of_a t =
        [%sexp_of: (t * a) list] (Stdlib.List.of_seq (Map.to_seq t))

      include Map
    end
  end

  type var = Ident.t [@@deriving sexp_of]

  type t = Shape.t =
    { uid : Uid.t option
    ; desc : Shape.desc
    ; approximated : bool
    }

  let sexp_of_t_ref = ref (fun _ -> assert false)
  let sexp_of_t t = !sexp_of_t_ref t

  type desc = Shape.desc =
    | Var of var
    | Abs of var * t
    | App of t * t
    | Struct of t Item.Map.t
    | Alias of t
    | Leaf
    | Proj of t * Item.t
    | Comp_unit of string
    | Error of string
  [@@deriving sexp_of]

  type t2 =
    { uid : Uid.t option [@sexp.option]
    ; approximated : bool
          [@sexp.default false] [@sexp_drop_default.equal] (* display this before desc *)
    ; desc : desc
    }
  [@@deriving sexp_of]

  let () =
    sexp_of_t_ref :=
      fun { uid; desc; approximated } -> sexp_of_t2 { uid; desc; approximated }
end

module Shape_reduce = struct
  include Shape_reduce

  type result = Shape_reduce.result =
    | Resolved of Shape.Uid.t
    | Resolved_alias of Shape.Uid.t * result
    | Unresolved of Shape.t
    | Approximated of Shape.Uid.t option
    | Internal_error_missing_uid
  [@@deriving sexp_of]
end

module Location = struct
  include Location

  type position = Lexing.position =
    { pos_fname : string
    ; pos_lnum : int
    ; pos_bol : int
    ; pos_cnum : int
    }

  let sexp_of_position { pos_fname; pos_lnum; pos_bol; pos_cnum } =
    sexp_of_string
      (Printf.sprintf "%s:%d:%d (%d)" pos_fname pos_lnum (pos_cnum - pos_bol) pos_cnum)

  let compare_position p1 p2 =
    (* pos_cnum and pos_bol are shifted because, I think, of the line directive we used
       to ensure pos_fname is the same. *)
    [%compare: string * int * int]
      (p1.pos_fname, p1.pos_lnum, p1.pos_cnum - p1.pos_bol)
      (p2.pos_fname, p2.pos_lnum, p2.pos_cnum - p2.pos_bol)

  let hash_fold_position acc p =
    let acc = String.hash_fold_t acc p.pos_fname in
    let acc = Int.hash_fold_t acc p.pos_lnum in
    let acc = Int.hash_fold_t acc (p.pos_cnum - p.pos_bol) in
    acc

  let hash_position p =
    Ppx_hash_lib.Std.Hash.get_hash_value
      (hash_fold_position (Ppx_hash_lib.Std.Hash.create ()) p)

  type t = Location.t =
    { loc_start : position
    ; loc_end : position
    ; loc_ghost : bool
    }
  [@@deriving sexp_of]

  type 'a loc = 'a Location.loc =
    { txt : 'a
    ; loc : t
    }
  [@@deriving sexp_of]

  module Including_filename = struct
    type nonrec position = position [@@deriving sexp_of, compare, hash]

    type t = Location.t =
      { loc_start : position
      ; loc_end : position
      ; loc_ghost : bool
      }
    [@@deriving compare, hash, sexp_of]

    type 'a loc = 'a Location.loc =
      { txt : 'a
      ; loc : t
      }
    [@@deriving compare, sexp_of]
  end

  module Ignoring_filename = struct
    type nonrec position = position

    let sexp_of_position = sexp_of_position
    let compare_position p1 p2 = compare_position p1 { p2 with pos_fname = p1.pos_fname }
    let hash_fold_position acc p = hash_fold_position acc { p with pos_fname = "" }
    let hash_position p = hash_position { p with pos_fname = "" }

    type t = Location.t =
      { loc_start : position
      ; loc_end : position
      ; loc_ghost : bool
      }
    [@@deriving compare, hash, sexp_of]

    type 'a loc = 'a Location.loc =
      { txt : 'a
      ; loc : t
      }
    [@@deriving compare, sexp_of]
  end
end

module Longident_loc_ignoring_filename = struct
  type t = Longident.t Location.Ignoring_filename.loc [@@deriving compare, sexp_of]

  include (val Comparator.make ~compare ~sexp_of_t)
end

include struct
  (* taken from grep_cmt, https://github.com/LexiFi/grep_cmt/blob/main/grep_cmt.ml *)
  let initial_env = lazy (Compmisc.initial_env ())

  let type_type t =
    try
      let env = Lazy.force initial_env in
      (Typetexp.transl_type_scheme env t).ctyp_type
    with exn ->
      failwith (Format.asprintf "could not parse type: %a." Location.report_exception exn)

  let match_typ ~env texpr ~user_type:typ =
    try Ctype.is_moregeneral env false typ texpr
    with Assert_failure _ ->
      (* When dealing with inline records [moregeneral] above calls
         [Env.find_type_full] in a context that should never occur in the
         typechecker which causes assert false to be thrown. *)
      false
end

module Env_summary = struct
  type t = Env.summary =
    | Env_empty
    | Env_value of t * Ident.t * (Types.value_description[@sexp.opaque])
    | Env_type of t * Ident.t * (Types.type_declaration[@sexp.opaque])
    | Env_extension of t * Ident.t * (Types.extension_constructor[@sexp.opaque])
    | Env_module of
        t
        * Ident.t
        * (Types.module_presence[@sexp.opaque])
        * (Types.module_declaration[@sexp.opaque])
    | Env_modtype of t * Ident.t * (Types.modtype_declaration[@sexp.opaque])
    | Env_class of t * Ident.t * (Types.class_declaration[@sexp.opaque])
    | Env_cltype of t * Ident.t * (Types.class_type_declaration[@sexp.opaque])
    | Env_open of t * Path.t
    | Env_functor_arg of t * Ident.t
    | Env_constraints of t * (Types.type_declaration Path.Map.t[@sexp.opaque])
    | Env_copy_types of t
    | Env_persistent of t * Ident.t
    | Env_value_unbound of t * string * (Env.value_unbound_reason[@sexp.opaque])
    | Env_module_unbound of t * string * (Env.module_unbound_reason[@sexp.opaque])

  let rec sexps_of_t acc = function
    | Env_empty -> acc
    | Env_value (a, b, c) ->
        sexps_of_t
          ([%sexp `value, (b : Ident.t), (c : (Types.value_description[@sexp.opaque]))]
          :: acc)
          a
    | Env_type (a, b, c) ->
        sexps_of_t
          ([%sexp `type_, (b : Ident.t), (c : (Types.type_description[@sexp.opaque]))]
          :: acc)
          a
    | Env_extension (a, b, c) ->
        sexps_of_t
          ([%sexp
             `extension, (b : Ident.t), (c : (Types.extension_constructor[@sexp.opaque]))]
          :: acc)
          a
    | Env_module (a, b, c, d) ->
        sexps_of_t
          ([%sexp
             `module_
           , (b : Ident.t)
           , (c : (Types.module_presence[@sexp.opaque]))
           , (d : (Types.module_declaration[@sexp.opaque]))]
          :: acc)
          a
    | Env_modtype (a, b, c) ->
        sexps_of_t
          ([%sexp
             `modtype, (b : Ident.t), (c : (Types.modtype_declaration[@sexp.opaque]))]
          :: acc)
          a
    | Env_class (a, b, c) ->
        sexps_of_t
          ([%sexp `class_, (b : Ident.t), (c : (Types.class_declaration[@sexp.opaque]))]
          :: acc)
          a
    | Env_cltype (a, b, c) ->
        sexps_of_t
          ([%sexp
             `cltype, (b : Ident.t), (c : (Types.class_type_declaration[@sexp.opaque]))]
          :: acc)
          a
    | Env_open (a, b) -> sexps_of_t ([%sexp `open_, (b : Path.t)] :: acc) a
    | Env_functor_arg (a, b) -> sexps_of_t ([%sexp `functor_arg, (b : Ident.t)] :: acc) a
    | Env_constraints (a, b) ->
        sexps_of_t
          ([%sexp `constraints, (b : (Types.type_declaration Path.Map.t[@sexp.opaque]))]
          :: acc)
          a
    | Env_copy_types a -> sexps_of_t ([%sexp `copy_types] :: acc) a
    | Env_persistent (a, b) -> sexps_of_t ([%sexp `persistent, (b : Ident.t)] :: acc) a
    | Env_value_unbound (a, b, c) ->
        sexps_of_t
          ([%sexp
             `value_unbound, (b : string), (c : (Env.value_unbound_reason[@sexp.opaque]))]
          :: acc)
          a
    | Env_module_unbound (a, b, c) ->
        sexps_of_t
          ([%sexp
             `module_unbound
           , (b : string)
           , (c : (Env.module_unbound_reason[@sexp.opaque]))]
          :: acc)
          a

  let sexp_of_t s = Sexp.List (sexps_of_t [] s)

  let next = function
    | Env_empty -> None
    | Env_value (a, _, _)
    | Env_type (a, _, _)
    | Env_extension (a, _, _)
    | Env_module (a, _, _, _)
    | Env_modtype (a, _, _)
    | Env_class (a, _, _)
    | Env_cltype (a, _, _)
    | Env_open (a, _)
    | Env_functor_arg (a, _)
    | Env_constraints (a, _)
    | Env_copy_types a
    | Env_persistent (a, _)
    | Env_value_unbound (a, _, _)
    | Env_module_unbound (a, _, _) ->
        Some a

  let next_exn t =
    match next t with None -> invalid_arg "Env_summar.next_exn" | Some t -> t

  let set_exn t ~next =
    match t with
    | Env_empty -> invalid_arg "set_exn"
    | Env_value (_, b, c) -> Env_value (next, b, c)
    | Env_type (_, b, c) -> Env_type (next, b, c)
    | Env_extension (_, b, c) -> Env_extension (next, b, c)
    | Env_module (_, b, c, d) -> Env_module (next, b, c, d)
    | Env_modtype (_, b, c) -> Env_modtype (next, b, c)
    | Env_class (_, b, c) -> Env_class (next, b, c)
    | Env_cltype (_, b, c) -> Env_cltype (next, b, c)
    | Env_open (_, b) -> Env_open (next, b)
    | Env_functor_arg (_, b) -> Env_functor_arg (next, b)
    | Env_constraints (_, b) -> Env_constraints (next, b)
    | Env_copy_types _ -> Env_copy_types next
    | Env_persistent (_, b) -> Env_persistent (next, b)
    | Env_value_unbound (_, b, c) -> Env_value_unbound (next, b, c)
    | Env_module_unbound (_, b, c) -> Env_module_unbound (next, b, c)

  let length t =
    let rec loop acc t = match next t with None -> acc | Some t -> loop (acc + 1) t in
    loop 0 t

  let rec at_exn t i f =
    if i <= 0 then f t else set_exn t ~next:(at_exn (next_exn t) (i - 1) f)

  let rebase ~old_base ~new_base =
    let len_old_base = length old_base in
    { next = (fun old -> at_exn old (length old - len_old_base) (fun _ -> new_base)) }

  let rebase' ~old_base ~new_base =
    let { next } =
      rebase ~old_base:(Env.summary old_base) ~new_base:(Env.summary new_base)
    in
    { next =
        (fun old ->
          Env.env_of_only_summary
            (fun old subst -> Envaux.env_from_summary (next old) subst)
            old)
    }
end

type env = Env.t

let sexp_of_env env =
  [%sexp
    { values : (string * Path.t * _) list =
        Env.fold_values (fun str path z acc -> (str, path, z) :: acc) None env []
    ; modules : (string * Path.t * _) list =
        Env.fold_modules (fun str path z acc -> (str, path, z) :: acc) None env []
    ; summary : Env_summary.t = Env.summary env
    }]

let decl_attributes (item_declaration : Typedtree.item_declaration) =
  match item_declaration with
  | Value v -> v.val_attributes
  | Value_binding v -> v.vb_attributes
  | Type v -> v.typ_attributes
  | Constructor v -> v.cd_attributes
  | Extension_constructor v -> v.ext_attributes
  | Label v -> v.ld_attributes
  | Module v -> v.md_attributes
  | Module_substitution v -> v.ms_attributes
  | Module_binding v -> v.mb_attributes
  | Module_type v -> v.mtd_attributes
  | Class v -> v.ci_attributes
  | Class_type v -> v.ci_attributes

let sigitem_attributes (signature_item : Types.signature_item) =
  match signature_item with
  | Sig_value (_, d, _) -> d.val_attributes
  | Sig_type (_, d, _, _) -> d.type_attributes
  | Sig_typext (_, d, _, _) -> d.ext_attributes
  | Sig_module (_, _, d, _, _) -> d.md_attributes
  | Sig_modtype (_, d, _) -> d.mtd_attributes
  | Sig_class (_, d, _, _) -> d.cty_attributes
  | Sig_class_type (_, d, _, _) -> d.clty_attributes

type 'a ns =
  | Value : (Path.t * Types.value_description) ns
  | Type : (Path.t * Types.type_declaration) ns
  | Module : (Path.t * Types.module_declaration) ns
  | Module_type : (Path.t * Types.modtype_declaration) ns
  | Class : (Path.t * Types.class_declaration) ns
  | Class_type : (Path.t * Types.class_type_declaration) ns
  | Constructor : Types.constructor_description ns
  | Label : Types.label_description ns

let find_by_name (type a) (ns : a ns) env lid : a =
  match ns with
  | Value -> Env.find_value_by_name lid env
  | Type -> Env.find_type_by_name lid env
  | Module -> Env.find_module_by_name lid env
  | Module_type -> Env.find_modtype_by_name lid env
  | Class -> Env.find_class_by_name lid env
  | Class_type -> Env.find_cltype_by_name lid env
  | Constructor -> Env.find_constructor_by_name lid env
  | Label -> Env.find_label_by_name lid env

let uid (type a) (ns : a ns) (v : a) =
  match ns with
  | Value -> (snd v).val_uid
  | Type -> (snd v).type_uid
  | Module -> (snd v).md_uid
  | Module_type -> (snd v).mtd_uid
  | Class -> (snd v).cty_uid
  | Class_type -> (snd v).clty_uid
  | Constructor -> v.cstr_uid
  | Label -> v.lbl_uid

type 'a without_type_based_disambiguation =
  | T : ('a, Path.t * _) Type_equal.t -> 'a without_type_based_disambiguation

let without_type_based_disambiguation (type a) (ns : a ns) :
    a without_type_based_disambiguation option =
  match ns with
  | Value -> Some (T T)
  | Type -> Some (T T)
  | Module -> Some (T T)
  | Module_type -> Some (T T)
  | Class -> Some (T T)
  | Class_type -> Some (T T)
  | Constructor -> None
  | Label -> None

type any_pattern = T : _ Typedtree.general_pattern -> any_pattern
