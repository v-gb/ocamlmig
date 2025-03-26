open Base
module Filename = Stdlib.Filename
module Sys = Stdlib.Sys
module Printf = Stdlib.Printf
module Format = Stdlib.Format
open Common
module Parsetree = Ocamlformat_parser_extended.Parsetree

let ocaml_version (fmconf : Ocamlformat_lib.Conf.t) = fmconf.opr_opts.ocaml_version.v

let ocaml_version' fmconf =
  let ocaml_version = ocaml_version fmconf in
  Ocaml_version.(major ocaml_version, minor ocaml_version)

let maybe_add_debug conf =
  let open Ocamlformat_lib in
  if not debug.ocamlformat
  then conf
  else
    Conf.Operational.update conf ~f:(fun opr_opts ->
        { opr_opts with debug = Conf_t.Elt.make true `Default })

let ocamlformat_print =
  let open Ocamlformat_format in
  let open Ocamlformat_lib in
  let with_buffer_formatter ~buffer_size k =
    let buffer = Buffer.create buffer_size in
    let fs = Format_.formatter_of_buffer buffer in
    Fmt.eval fs k;
    Format_.pp_print_flush fs ();
    if Buffer.length buffer > 0 then Format_.pp_print_newline fs ();
    Buffer.contents buffer
  in
  fun (type ext) (ext_fg : ext Extended_ast.t) ~(conf : Conf.t)
      (ext_t : _ Parse_with_comments.with_comments) ->
    Profile.record "fmast_print" (fun () ->
        let open Fmt in
        let conf = maybe_add_debug conf in
        let cmts_t =
          Ocamlformat_lib.Cmts.init ext_fg ~debug:conf.opr_opts.debug.v ext_t.source
            ext_t.ast ext_t.comments
        in
        let contents =
          with_buffer_formatter ~buffer_size:1000
            (set_margin conf.fmt_opts.margin.v
            $ set_max_indent conf.fmt_opts.max_indent.v
            $ fmt_if
                (not (String.is_empty ext_t.prefix))
                (str ext_t.prefix $ force_newline)
            $ Fmt_ast.fmt_ast ext_fg ~debug:conf.opr_opts.debug.v ext_t.source cmts_t conf
                ext_t.ast)
        in
        contents)

let fake_pos =
  let super = Ocamlformat_parser_extended.Ast_mapper.default_mapper in
  { super with
    location =
      (fun _self loc ->
        { loc_start = { loc.loc_start with pos_fname = "_migrate_import" }
        ; loc_end = { loc.loc_end with pos_fname = "_migrate_import" }
        ; loc_ghost = loc.loc_ghost
        })
  }

let debug_print (type a) ?(raw = false) (kind : a Ocamlformat_lib.Extended_ast.t)
    ?(fmconf = maybe_add_debug Ocamlformat_lib.Conf.default) (ast : a) =
  if raw
  then
    match kind with
    | Structure ->
        Format.asprintf "%a" Ocamlformat_parser_extended.Printast.implementation ast
    | Expression ->
        Format.asprintf "%a" Ocamlformat_parser_extended.Printast.expression ast
    | Core_type -> Format.asprintf "%a" Ocamlformat_parser_extended.Printast.core_type ast
    | _ -> failwith "unimplemented parsing"
  else
    ocamlformat_print kind ~conf:fmconf
      { source = Ocamlformat_lib.Source.create ~text:"" ~tokens:[]
      ; comments = []
      ; prefix = ""
      ; ast =
          (* Put a fake pos, otherwise ocamlformat looks for the source literals in the
             source (the empty string above), and fails. The fake pos helps because we
             have the ocamlformat linked is modified to not look in the source for such
             pos. *)
          Ocamlformat_lib.Extended_ast.map kind fake_pos ast
      }

let parse_with_ocamlformat kind ~conf ~input_name str =
  Profile.record "fmast_parse" (fun () ->
      Ocamlformat_lib.Parse_with_comments.parse
        ~disable_w50:true (* avoid exception being thrown *)
        (Ocamlformat_lib.Parse_with_comments.parse_ast conf)
        kind conf ~input_name ~source:str)

let update_structure (ast_plus : _ Ocamlformat_lib.Parse_with_comments.with_comments) f =
  Option.map (f ast_plus.ast) ~f:(fun (ast, other_stuff) ->
      ({ ast_plus with ast }, other_stuff))

open! Ocamlformat_ocaml_common
open Ocamlformat_parser_extended

module Longident = struct
  include Longident

  let compare : t -> t -> int = Stdlib.compare
  let sexp_of_t t = [%sexp (String.concat ~sep:"." (flatten t) : string)]

  include (val Comparator.make ~compare ~sexp_of_t)

  let map_modpath t f =
    match t with
    | Lident _ | Lapply _ -> t
    | Ldot (ident_path, field) -> Ldot (f ident_path, field)
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
  [@@deriving compare, hash, sexp_of]

  type 'a loc = 'a Location.loc =
    { txt : 'a
    ; loc : t
    }
  [@@deriving sexp_of, compare]

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

module Ast_helper = struct
  include Ast_helper

  let located ?(loc = !default_loc) txt : _ Location.loc = { txt; loc }

  module Pat = struct
    include Pat

    let some ?(loc = !default_loc) arg =
      construct ~loc (located ~loc (Longident.Lident "Some")) (Some ([], arg))

    let none ?(loc = !default_loc) () =
      construct ~loc (located ~loc (Longident.Lident "None")) None

    let ext_exp ?(loc = !default_loc) name e =
      Ast_helper.Pat.extension ({ txt = name; loc }, PStr [ Ast_helper.Str.eval e ])
  end

  module Exp = struct
    include Exp

    let ident' ?loc longident = ident ?loc (located ?loc longident)

    let let' ?loc ?(rec_ = false) bindings ~loc_in body =
      match bindings with
      | [] -> body
      | _ :: _ ->
          let_ ?loc ~loc_in
            { pvbs_rec = (if rec_ then Recursive else Nonrecursive)
            ; pvbs_bindings = bindings
            }
            body

    let string ?(loc = !default_loc) ?quotation_delimiter s =
      constant ~loc (Const.string ?quotation_delimiter ~loc s)

    let int ?(loc = !default_loc) ?suffix s = constant ~loc (Const.int ?suffix ~loc s)

    let some ?(loc = !default_loc) arg =
      construct ~loc (located ~loc (Longident.Lident "Some")) (Some arg)

    let none ?(loc = !default_loc) () =
      construct ~loc (located ~loc (Longident.Lident "None")) None

    let true_ ?(loc = !default_loc) () =
      construct ~loc (located ~loc (Longident.Lident "true")) None

    let false_ ?(loc = !default_loc) () =
      construct ~loc (located ~loc (Longident.Lident "false")) None

    let unit ?(loc = !default_loc) ?attrs () =
      construct ~loc ?attrs (located ~loc (Longident.Lident "()")) None

    let tuple ?loc ?attrs l =
      match l with
      | [] -> unit ?loc ?attrs ()
      | [ e ] -> e
      | _ :: _ :: _ -> tuple ?loc ?attrs l

    let ext_exp ?(loc = !default_loc) name e =
      Ast_helper.Exp.extension ({ txt = name; loc }, PStr [ Ast_helper.Str.eval e ])
  end

  module Attr = struct
    include Attr

    let exp ?(loc = !default_loc) name e =
      Ast_helper.Attr.mk (located ~loc name) (PStr [ Str.eval ~loc e ])
  end
end

type function_arg = Asttypes.arg_label * Parsetree.expression

let pp_function_args ?raw ppf l =
  Format.fprintf ppf "%s"
    (debug_print ?raw Expression (Ast_helper.Exp.apply (Ast_helper.Exp.unreachable ()) l))

let pp_function_arg ?raw ppf arg = pp_function_args ?raw ppf [ arg ]

type expression = Parsetree.expression
type pattern = Parsetree.pattern
type structure_item = Parsetree.structure_item
type structure = Parsetree.structure
type core_type = Parsetree.core_type
type class_field = Parsetree.class_field
type class_type = Parsetree.class_type
type module_expr = Parsetree.module_expr
type module_type = Parsetree.module_type
type signature_item = Parsetree.signature_item
type signature = Parsetree.signature

let sexp_of_extended_ast ?raw ext v =
  sexp_of_string (String.chop_suffix_if_exists (debug_print ?raw ext v) ~suffix:"\n")

let sexp_of_expression ?raw v = sexp_of_extended_ast ?raw Expression v
let sexp_of_pattern v = sexp_of_extended_ast Pattern v
let sexp_of_structure_item v = sexp_of_extended_ast Structure [ v ]
let sexp_of_structure v = sexp_of_extended_ast Structure v
let sexp_of_core_type v = sexp_of_extended_ast Core_type v
let sexp_of_class_field v = sexp_of_extended_ast Class_field v
let sexp_of_class_type v = sexp_of_extended_ast Class_type v
let sexp_of_module_expr v = sexp_of_extended_ast Module_expr v
let sexp_of_module_type v = sexp_of_extended_ast Module_type v
let sexp_of_signature_item v = sexp_of_extended_ast Signature [ v ]
let sexp_of_signature v = sexp_of_extended_ast Signature v

type arg_label = Asttypes.arg_label

module Arg_label = struct
  type t = arg_label

  let sexp_of_t : t -> _ = function
    | Nolabel -> [%sexp Nolabel]
    | Labelled s -> [%sexp Labelled (s.txt : string)]
    | Optional s -> [%sexp Optional (s.txt : string)]

  let equal (t1 : t) (t2 : t) =
    match (t1, t2) with
    | Nolabel, Nolabel -> true
    | Labelled s1, Labelled s2 -> s1.txt =: s2.txt
    | Optional s1, Optional s2 -> s1.txt =: s2.txt
    | (Nolabel | Labelled _ | Optional _), _ -> false

  let to_string : t -> _ = function Nolabel -> "" | Labelled s | Optional s -> s.txt
end

let sexp_of_arg_label = Arg_label.sexp_of_t

module P = Parsetree

let type_constraint_of_value_constraint : P.value_constraint -> P.type_constraint =
  function
  | Pvc_constraint { locally_abstract_univars; typ } ->
      (* not syntactically possible to write let f x : type a. a = x*)
      assert (List.is_empty locally_abstract_univars);
      Pconstraint typ
  | Pvc_coercion { ground; coercion } -> Pcoerce (ground, coercion)

let value_constraint_of_type_constraint : P.type_constraint -> P.value_constraint =
  function
  | Pconstraint typ -> Pvc_constraint { locally_abstract_univars = []; typ }
  | Pcoerce (ground, coercion) -> Pvc_coercion { ground; coercion }

module Node = struct
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

  let loc (type w a b e) (t : (w, a, b, e) t) (v : a) =
    match t with
    | Exp -> v.pexp_loc
    | Pat -> v.ppat_loc
    | Typ -> v.ptyp_loc
    | Mexp -> v.pmod_loc
    | Mtyp -> v.pmty_loc
    | Cexp -> v.pcl_loc
    | Ctyp -> v.pcty_loc
    | Value_binding -> v.pvb_loc
    | Binding_op -> v.pbop_loc

  let attributes (type w a b e) (t : (w, a, b, e) t) (v : a) =
    match t with
    | Exp -> v.pexp_attributes
    | Pat -> v.ppat_attributes
    | Typ -> v.ptyp_attributes
    | Mexp -> v.pmod_attributes
    | Mtyp -> v.pmty_attributes
    | Cexp -> v.pcl_attributes
    | Ctyp -> v.pcty_attributes
    | Value_binding -> v.pvb_attributes.attrs_before @ v.pvb_attributes.attrs_after
    | Binding_op -> []

  let desc (type a b e) (t : (desc, a, b, e) t) (v : a) : b =
    match t with
    | Exp -> v.pexp_desc
    | Pat -> v.ppat_desc
    | Typ -> v.ptyp_desc
    | Mexp -> v.pmod_desc
    | Mtyp -> v.pmty_desc
    | Cexp -> v.pcl_desc
    | Ctyp -> v.pcty_desc

  let update (type a b e) ?(desc : b option) ?attributes (t : (desc, a, b, e) t) (v : a) :
      a =
    match t with
    | Exp ->
        { v with
          pexp_desc = Option.value desc ~default:v.pexp_desc
        ; pexp_attributes = Option.value attributes ~default:v.pexp_attributes
        }
    | Pat ->
        { v with
          ppat_desc = Option.value desc ~default:v.ppat_desc
        ; ppat_attributes = Option.value attributes ~default:v.ppat_attributes
        }
    | Typ ->
        { v with
          ptyp_desc = Option.value desc ~default:v.ptyp_desc
        ; ptyp_attributes = Option.value attributes ~default:v.ptyp_attributes
        }
    | Mexp ->
        { v with
          pmod_desc = Option.value desc ~default:v.pmod_desc
        ; pmod_attributes = Option.value attributes ~default:v.pmod_attributes
        }
    | Mtyp ->
        { v with
          pmty_desc = Option.value desc ~default:v.pmty_desc
        ; pmty_attributes = Option.value attributes ~default:v.pmty_attributes
        }
    | Cexp ->
        { v with
          pcl_desc = Option.value desc ~default:v.pcl_desc
        ; pcl_attributes = Option.value attributes ~default:v.pcl_attributes
        }
    | Ctyp ->
        { v with
          pcty_desc = Option.value desc ~default:v.pcty_desc
        ; pcty_attributes = Option.value attributes ~default:v.pcty_attributes
        }

  let meth (type w a b e) (t : (w, a, b, e) t) (mapper : Ast_mapper.mapper) :
      Ast_mapper.mapper -> a -> a =
    match t with
    | Exp -> mapper.expr
    | Pat -> mapper.pat
    | Typ -> mapper.typ
    | Mexp -> mapper.module_expr
    | Mtyp -> mapper.module_type
    | Cexp -> mapper.class_expr
    | Ctyp -> mapper.class_type
    | Value_binding -> mapper.value_binding
    | Binding_op -> mapper.binding_op

  let map t mapper v = (meth t mapper) mapper v
end

module Flat_longident = struct
  type t = string * cont list

  and cont =
    | Dot of string
    | Apply_to of t
  [@@deriving sexp_of, equal, compare, hash]

  let rec from_longident conts : Longident.t -> t = function
    | Lident s -> (s, conts)
    | Ldot (t, s) -> from_longident (Dot s :: conts) t
    | Lapply (t1, t2) -> from_longident (Apply_to (from_longident [] t2) :: conts) t1

  let from_longident id = from_longident [] id

  let rec to_longident (s, conts) =
    List.fold_left conts ~init:(Longident.Lident s) ~f:(fun acc cont ->
        match cont with
        | Dot s -> Ldot (acc, s)
        | Apply_to arg -> Lapply (acc, to_longident arg))

  let to_list (s, conts) = Dot s :: conts

  let is_prefix t1 ~prefix:t2 =
    List.is_prefix (to_list t1) ~prefix:(to_list t2) ~equal:equal_cont

  let chop_prefix t1 ~prefix =
    if is_prefix t1 ~prefix
    then
      match List.drop (to_list t1) (List.length (to_list prefix)) with
      | Dot s :: conts -> Some (s, conts)
      | _ -> None
    else None
end
