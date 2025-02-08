open Base
module Filename = Stdlib.Filename
module Sys = Stdlib.Sys
module Printf = Stdlib.Printf
module Format = Stdlib.Format
open Common
module Parsetree = Ocamlformat_parser_extended.Parsetree

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
    let open Fmt in
    let conf = maybe_add_debug conf in
    let cmts_t =
      Ocamlformat_lib.Cmts.init ext_fg ~debug:conf.opr_opts.debug.v ext_t.source ext_t.ast
        ext_t.comments
    in
    let contents =
      with_buffer_formatter ~buffer_size:1000
        (set_margin conf.fmt_opts.margin.v
        $ set_max_indent conf.fmt_opts.max_indent.v
        $ fmt_if (not (String.is_empty ext_t.prefix)) (str ext_t.prefix $ force_newline)
        $ Fmt_ast.fmt_ast ext_fg ~debug:conf.opr_opts.debug.v ext_t.source cmts_t conf
            ext_t.ast)
    in
    contents

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
          (match kind with
          | Structure -> fake_pos.structure fake_pos ast
          | Signature -> fake_pos.signature fake_pos ast
          | Use_file -> ast
          | Core_type -> fake_pos.typ fake_pos ast
          | Module_type -> fake_pos.module_type fake_pos ast
          | Expression -> fake_pos.expr fake_pos ast
          | Repl_file -> ast
          | Documentation -> ast)
      }

let debug_print_pattern =
  let open Ocamlformat_parser_extended in
  fun ?fmconf (ast : Parsetree.pattern) ->
    let open Ast_helper in
    debug_print ?fmconf Expression
      (Exp.function_
         [ { pparam_desc = Pparam_val (Nolabel, None, ast); pparam_loc = ast.ppat_loc } ]
         None
         (Pfunction_body (Exp.unreachable ())))
    |> String.chop_prefix_if_exists ~prefix:"fun "
    |> String.chop_suffix_if_exists ~suffix:"\n"
    |> String.chop_suffix_if_exists ~suffix:" -> ."

let _ = debug_print_pattern

let parse_with_ocamlformat kind ~conf ~input_name str =
  Ocamlformat_lib.Parse_with_comments.parse
    ~disable_w50:true (* avoid exception being thrown *)
    (Ocamlformat_lib.Parse_with_comments.parse_ast conf)
    kind conf ~input_name ~source:str

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

    let some ?(loc = !default_loc) arg =
      construct ~loc (located ~loc (Longident.Lident "Some")) (Some arg)

    let none ?(loc = !default_loc) () =
      construct ~loc (located ~loc (Longident.Lident "None")) None

    let true_ ?(loc = !default_loc) () =
      construct ~loc (located ~loc (Longident.Lident "true")) None

    let false_ ?(loc = !default_loc) () =
      construct ~loc (located ~loc (Longident.Lident "false")) None

    let unit ?(loc = !default_loc) () =
      construct ~loc (located ~loc (Longident.Lident "()")) None
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

let sexp_of_expression ?raw e =
  sexp_of_string
    (String.chop_suffix_if_exists (debug_print ?raw Expression e) ~suffix:"\n")

let sexp_of_pattern p =
  sexp_of_string (String.chop_suffix_if_exists (debug_print_pattern p) ~suffix:"\n")

let sexp_of_structure_item s =
  sexp_of_string (String.chop_suffix_if_exists (debug_print Structure [ s ]) ~suffix:"\n")

let sexp_of_structure l =
  sexp_of_string (String.chop_suffix_if_exists (debug_print Structure l) ~suffix:"\n")

let sexp_of_core_type t =
  sexp_of_string (String.chop_suffix_if_exists (debug_print Core_type t) ~suffix:"\n")

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
