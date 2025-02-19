open! Base
open! Stdio
open! Common

module File_type = struct
  type _ t =
    | Intf : Ocamlformat_parser_extended.Parsetree.signature t
    | Impl : Ocamlformat_parser_extended.Parsetree.structure t

  type packed = T : _ t -> packed

  let to_extended_ast (type a) : a t -> a Ocamlformat_lib.Extended_ast.t = function
    | Intf -> Ocamlformat_lib.Extended_ast.Signature
    | Impl -> Ocamlformat_lib.Extended_ast.Structure

  let map (type a) (typed : a t) (mapper : Ocamlformat_parser_extended.Ast_mapper.mapper)
      (v : a) : a =
    match typed with
    | Intf -> mapper.signature mapper v
    | Impl -> mapper.structure mapper v

  let method_ (type a) (typed : a t)
      (mapper : Ocamlformat_parser_extended.Ast_mapper.mapper) :
      Ocamlformat_parser_extended.Ast_mapper.mapper -> a -> a =
    match typed with Intf -> mapper.signature | Impl -> mapper.structure

  let structure (type a) (t : a t) (a : a) :
      Ocamlformat_parser_extended.Parsetree.structure option =
    match t with Intf -> None | Impl -> Some a
end

let migrate_filename_import = "_migrate_import"
let migrate_filename_gen = "_migrate_gen"

let migrate_filename = function
  | `Import -> migrate_filename_import
  | `Gen -> migrate_filename_gen

open! Ocamlformat_ocaml_common
open Ocamlformat_parser_extended
module P = Parsetree
open Fmast

let is_migrate_filename (loc : Location.t) =
  loc.loc_start.pos_fname =: migrate_filename_gen
  || loc.loc_start.pos_fname =: migrate_filename_import

let migrate_loc source = Location.in_file (migrate_filename source)

let with_log f =
  ();
  fun (self : Ast_mapper.mapper) (expr : P.expression) ->
    if
      List.exists expr.pexp_attributes ~f:(fun attr ->
          attr.attr_name.txt =: "migrate.log")
    then Ref.set_temporarily log true ~f:(fun () -> f self expr)
    else f self expr

module Attr = struct
  type t = P.attributes

  let find (t : t) name = List.find t ~f:(fun attr -> attr.attr_name.txt =: name)
  let exists (t : t) name = List.exists t ~f:(fun attr -> attr.attr_name.txt =: name)
  let remove (t : t) name = List.filter t ~f:(fun attr -> attr.attr_name.txt <>: name)

  let _add_if_absent (t : t) (attr : P.attribute) =
    if exists t attr.attr_name.txt then t else attr :: t

  let prefix = "miginternal_"

  let reorder which =
    match which with `Source -> "reorder" | `Internal -> prefix ^ "reorder"

  let create ~loc name payload : P.attribute =
    { attr_name = { loc; txt = prefix ^ name }
    ; attr_payload =
        PStr
          (match payload with
          | None -> []
          | Some e -> [ { pstr_desc = Pstr_eval (e, []); pstr_loc = loc } ])
    ; attr_loc = loc
    }
end

module Sattr = struct
  type 'a t =
    { name : string
    ; build : loc:Location.t -> 'a -> P.attribute
    ; match_ : P.attribute -> 'a
    }

  let find t attrs = Option.map (Attr.find attrs t.name) ~f:t.match_
  let exists t attrs = Attr.exists attrs t.name
  let _remove t attrs = Attr.remove attrs t.name

  let match_expr match_ =
    ();
    function
    | { P.attr_payload = PStr [ { pstr_desc = Pstr_eval (e, []); _ } ]; _ } -> match_ e
    | _ -> assert false

  let int_attr name =
    { name = Attr.prefix ^ name
    ; build =
        (fun ~loc id ->
          Attr.create ~loc name (Some (Ast_helper.Exp.constant (Ast_helper.Const.int id))))
    ; match_ =
        match_expr (function
          | { pexp_desc = Pexp_constant { pconst_desc = Pconst_integer (id, None); _ }
            ; _
            } ->
              Int.of_string id
          | _ -> assert false)
    }

  let id = int_attr "id"

  let pref =
    { name = Attr.prefix ^ "pref"
    ; build = (fun ~loc e -> Attr.create ~loc "pref" (Some e))
    ; match_ = match_expr Fn.id
    }

  let pun =
    { name = Attr.prefix ^ "pun"
    ; build = (fun ~loc () -> Attr.create ~loc "pun" None)
    ; match_ = (fun _ -> ())
    }

  let touched =
    (* This is an alternative mechanism to detect ASTs we modified, in addition to
       the loc filename. Maybe we should try to only use the positions, but positions
       are used by ocamlformat for comment placement, so it's a bit delicate. *)
    { name = Attr.prefix ^ "touched"
    ; build = (fun ~loc () -> Attr.create ~loc "touched" None)
    ; match_ = (fun _ -> ())
    }

  let orig =
    { name = Attr.prefix ^ "orig"
    ; build = (fun ~loc e -> Attr.create ~loc "orig" (Some e))
    ; match_ = match_expr Fn.id
    }
end

let update_migrate_test_payload =
  let update_migrate_test ?(match_attr = ( =: ) "migrate_test") (si : P.structure_item) f
      ~default =
    match si.pstr_desc with
    | Pstr_value
        { pvbs_bindings = [ ({ pvb_attributes; _ } as pvs_binding) ]
        ; pvbs_rec = Nonrecursive
        } ->
        let found = ref false in
        let find_attr (attr : P.attribute) =
          match attr with
          | { attr_name = { txt; _ }; _ } when match_attr txt ->
              found := true;
              f attr
          | _ -> Some attr
        in
        let pvb_attributes_attrs_before =
          List.filter_map pvb_attributes.attrs_before ~f:find_attr
        in
        let pvb_attributes_attrs_after =
          List.filter_map pvb_attributes.attrs_after ~f:find_attr
        in
        if not !found
        then force default
        else
          let pvb_attributes =
            { pvb_attributes with
              attrs_after = pvb_attributes_attrs_after
            ; attrs_before = pvb_attributes_attrs_before
            }
          in
          let pvbs_bindings = [ { pvs_binding with pvb_attributes } ] in
          { si with pstr_desc = Pstr_value { pvbs_bindings; pvbs_rec = Nonrecursive } }
    | _ -> force default
  in
  fun ?match_attr ?(state = ref false) ~changed_something (super : Ast_mapper.mapper) ->
    ();
    fun self si ->
      let si' = lazy (super.structure_item self si) in
      update_migrate_test ?match_attr si ~default:si' (fun attr ->
          changed_something := true;
          Ref.set_temporarily state true ~f:(fun () ->
              Some
                { attr with
                  attr_payload =
                    PStr
                      [ update_migrate_test ?match_attr (force si')
                          (fun _ -> None)
                          ~default:si'
                      ]
                }))

let drop_concrete_syntax_constructs =
  let super = Ast_mapper.default_mapper in
  { super with
    pat =
      (fun self pat ->
        let pat = super.pat self pat in
        match pat with
        | { ppat_desc = Ppat_record (labels, z); _ } ->
            let labels =
              List.map labels ~f:(fun ((field_name, typ, pat_opt) as field_up) ->
                  if Option.is_none pat_opt
                  then
                    let loc = field_name.loc in
                    let by =
                      Ast_helper.Pat.var ~loc
                        { txt = Longident.last field_name.txt; loc }
                        ~attrs:[ Sattr.pun.build ~loc:!Ast_helper.default_loc () ]
                    in
                    (field_name, typ, Some by)
                  else field_up)
            in
            { pat with ppat_desc = Ppat_record (labels, z) }
        | _ -> pat)
  ; expr =
      (fun self expr ->
        let expr = super.expr self expr in
        match expr.pexp_desc with
        | Pexp_record (fields, orig) ->
            let changed_something = ref false in
            let fields =
              List.map fields ~f:(fun ((field_name, typ, expr_opt) as field_up) ->
                  if Option.is_none expr_opt
                  then (
                    let loc = field_name.loc in
                    let by =
                      Ast_helper.Exp.ident ~loc
                        { txt = Lident (Longident.last field_name.txt); loc }
                        ~attrs:[ Sattr.pun.build ~loc:!Ast_helper.default_loc () ]
                    in
                    changed_something := true;
                    (field_name, typ, Some by))
                  else field_up)
            in
            if !changed_something
            then { expr with pexp_desc = Pexp_record (fields, orig) }
            else expr
        | Pexp_prefix (op, e1) ->
            let fun_ =
              let loc = op.loc in
              Ast_helper.Exp.ident ~loc { txt = Lident op.txt; loc }
                ~attrs:[ Sattr.pun.build ~loc:!Ast_helper.default_loc () ]
            in
            { expr with pexp_desc = Pexp_apply (fun_, [ (Nolabel, e1) ]) }
        | Pexp_infix (op, e1, e2) ->
            let fun_ =
              let loc = op.loc in
              Ast_helper.Exp.ident ~loc { txt = Lident op.txt; loc }
                ~attrs:[ Sattr.pun.build ~loc:!Ast_helper.default_loc () ]
            in
            { expr with pexp_desc = Pexp_apply (fun_, [ (Nolabel, e1); (Nolabel, e2) ]) }
        | Pexp_open (modname, e) ->
            (* print_s [%sexp ~~(modname.txt : Longident.t), ~~(modname.loc : Location.t)]; *)
            { expr with
              pexp_desc =
                Pexp_letopen
                  ( Ast_helper.Opn.mk ~loc:expr.pexp_loc
                      (Ast_helper.Mod.ident ~loc:modname.loc modname)
                  , e )
            ; pexp_attributes =
                Sattr.pun.build ~loc:!Ast_helper.default_loc () :: expr.pexp_attributes
            }
        | _ -> expr)
  ; value_binding =
      (fun self binding ->
        let binding = super.value_binding self binding in
        match (binding.pvb_args, binding.pvb_body) with
        | [], Pfunction_body _ -> binding
        | (_ :: _ as params), body | params, (Pfunction_cases _ as body) ->
            let body =
              let loc = binding.pvb_loc in
              Ast_helper.Exp.function_ ~loc params
                (Option.map binding.pvb_constraint
                   ~f:Fmast.type_constraint_of_value_constraint)
                body
                ~attrs:[ Sattr.pun.build ~loc:!Ast_helper.default_loc () ]
            in
            { binding with
              pvb_args = []
            ; pvb_body = Pfunction_body body
            ; pvb_constraint = None
            })
  }

let undrop_concrete_syntax_constructs =
  let super = Ast_mapper.default_mapper in
  { super with
    pat =
      (fun self pat ->
        let pat = super.pat self pat in
        match pat with
        | { ppat_desc = Ppat_record (labels, z); _ } ->
            let labels =
              List.map labels ~f:(fun ((field_name, typ, pat_opt) as field_up) ->
                  match pat_opt with
                  | Some { ppat_desc = Ppat_var var; ppat_attributes; _ }
                    when var.txt =: Longident.last field_name.txt
                         && (Sattr.exists Sattr.pun ppat_attributes
                            || Sattr.exists Sattr.touched ppat_attributes) ->
                      (field_name, typ, None)
                  | _ -> field_up)
            in
            { pat with ppat_desc = Ppat_record (labels, z) }
        | _ -> pat)
  ; expr =
      (fun self expr ->
        let expr = super.expr self expr in
        match expr.pexp_desc with
        | Pexp_record (fields, orig) ->
            let changed_something = ref false in
            let fields =
              List.map fields ~f:(fun ((field_name, typ, expr_opt) as field_up) ->
                  match expr_opt with
                  | Some
                      { pexp_desc = Pexp_ident { txt = Lident var; _ }
                      ; pexp_attributes
                      ; _
                      }
                    when var =: Longident.last field_name.txt
                         && (Sattr.exists Sattr.pun pexp_attributes
                            || Sattr.exists Sattr.touched pexp_attributes) ->
                      changed_something := true;
                      (field_name, typ, None)
                  | _ -> field_up)
            in
            if !changed_something
            then { expr with pexp_desc = Pexp_record (fields, orig) }
            else expr
        | Pexp_apply
            ( ({ pexp_desc = Pexp_ident { txt = Lident op; loc }; _ } as fun_)
            , [ (Nolabel, e1); (Nolabel, e2) ] )
          when (Sattr.exists Sattr.pun fun_.pexp_attributes
               || Sattr.exists Sattr.touched fun_.pexp_attributes
               || is_migrate_filename loc)
               && Ocamlformat_lib.Std_longident.String_id.is_infix op ->
            { expr with pexp_desc = Pexp_infix ({ txt = op; loc }, e1, e2) }
        | Pexp_apply
            ( ({ pexp_desc = Pexp_ident { txt = Lident op; loc }; _ } as fun_)
            , [ (Nolabel, e1) ] )
          when (Sattr.exists Sattr.pun fun_.pexp_attributes
               || Sattr.exists Sattr.touched fun_.pexp_attributes
               || is_migrate_filename loc)
               && Ocamlformat_lib.Std_longident.String_id.is_prefix op ->
            { expr with pexp_desc = Pexp_prefix ({ txt = op; loc }, e1) }
        | Pexp_letopen ({ popen_expr = { pmod_desc = Pmod_ident modname; _ }; _ }, e)
          when Sattr.exists Sattr.pun expr.pexp_attributes ->
            { expr with pexp_desc = Pexp_open (modname, e) }
        | _ -> expr)
  ; value_binding =
      (fun self binding ->
        let binding = super.value_binding self binding in
        match (binding.pvb_args, binding.pvb_body, binding.pvb_constraint) with
        | ( []
          , Pfunction_body
              ({ pexp_desc = Pexp_function (params, constraint_, inner_body); _ } as
               outer_body)
          , None )
          when Sattr.exists Sattr.pun outer_body.pexp_attributes
               || Sattr.exists Sattr.touched outer_body.pexp_attributes
               || is_migrate_filename outer_body.pexp_loc
               || is_migrate_filename binding.pvb_loc ->
            { binding with
              pvb_args = params
            ; pvb_constraint =
                Option.map constraint_ ~f:Fmast.value_constraint_of_type_constraint
            ; pvb_body = inner_body
            }
        | _ -> binding)
  }

let is_internal_attribute (attr : P.attribute) =
  String.is_prefix ~prefix:Attr.prefix attr.attr_name.txt

let remove_attributes =
  let super = Ast_mapper.default_mapper in
  { super with
    attributes =
      (fun self attributes ->
        let attributes = super.attributes self attributes in
        if List.exists attributes ~f:is_internal_attribute
        then List.filter attributes ~f:(Fn.non is_internal_attribute)
        else attributes)
  }

let update_loc (loc : Location.t) update_pos : Location.t =
  { loc_start = update_pos loc.loc_start
  ; loc_end = update_pos loc.loc_end
  ; loc_ghost = loc.loc_ghost
  }

let rec map_tail (e : P.expression) f =
  match e with
  | { pexp_desc = Pexp_let (a, e, b); _ } ->
      { e with pexp_desc = Pexp_let (a, map_tail e f, b) }
  | { pexp_desc = Pexp_sequence (a, e); _ } ->
      { e with pexp_desc = Pexp_sequence (a, map_tail e f) }
  | { pexp_desc = Pexp_letmodule (a, b, c, e); _ } ->
      { e with pexp_desc = Pexp_letmodule (a, b, c, map_tail e f) }
  | { pexp_desc = Pexp_letexception (a, e); _ } ->
      { e with pexp_desc = Pexp_letexception (a, map_tail e f) }
  | { pexp_desc = Pexp_open (a, e); _ } ->
      { e with pexp_desc = Pexp_open (a, map_tail e f) }
  | { pexp_desc = Pexp_letopen (a, e); _ } ->
      { e with pexp_desc = Pexp_letopen (a, map_tail e f) }
  | { pexp_desc = Pexp_beginend e; _ } ->
      { e with pexp_desc = Pexp_beginend (map_tail e f) }
  | { pexp_desc = Pexp_parens e; _ } -> { e with pexp_desc = Pexp_parens (map_tail e f) }
  | _ -> f e

let preserve_loc_to_preserve_comment_pos meth ~from to_ =
  let super = Ast_mapper.default_mapper in
  let self =
    { super with
      location =
        (fun _self loc ->
          if not (is_migrate_filename loc)
          then loc
          else
            let pos_fname = loc.loc_start.pos_fname in
            update_loc from (fun pos -> { pos with pos_fname }))
    }
  in
  (meth self) self to_

let preserve_loc_to_preserve_comment_pos_expr ~(from : P.expression) to_ =
  let rec loop (e : P.expression) ~saw_funs =
    map_tail e (fun e ->
        match e with
        | { pexp_desc = Pexp_function (params, tyopt, Pfunction_body body); _ } ->
            { e with
              pexp_desc =
                Pexp_function (params, tyopt, Pfunction_body (loop body ~saw_funs:true))
            }
        | { pexp_desc = Pexp_apply (fun_, args); _ } when saw_funs ->
            { e with pexp_desc = Pexp_apply (loop fun_ ~saw_funs:false, args) }
        | { pexp_desc = Pexp_ident _; _ } when not saw_funs ->
            { e with pexp_loc = from.pexp_loc }
        | _ -> e)
  in
  let to_ = loop to_ ~saw_funs:false in
  (* I am not entirely sure how ocamlformat places comments, but what's for sure is it
     relies on the positions on the AST to decide where to place things, and to find
     back AST nodes.

     A concrete problem I observed (preserved in a test) is that, when rewriting
     List.iter, the comments next to the second occurrence of List.iter get moved to
     the first occurrence of List.iter. I am unsure as to why exactly (I thought it was
     because the positions of the reinserted List.iter were identical, but enough adding
     noise to the loc_start.pos_lnum of all AST nodes to make them unique doesn't solve
     the problem.The simpler solution is what I do here: just stick all new AST nodes at
     the location of the original AST, this way different occurrences have different
     positions, and they have a location that's mostly consistent with their
     surroundings.

     It seems that the first loop above should be unnecessary given this one, but
     while it only changes the filename of one node, that makes a difference to
     comment placement for some reason.
   *)
  preserve_loc_to_preserve_comment_pos __.expr ~from:from.pexp_loc to_

type ast_result =
  | T :
      ('ast File_type.t
      * 'ast Ocamlformat_lib.Parse_with_comments.with_comments
      * 'ast Ocamlformat_lib.Parse_with_comments.with_comments
      * Ocamlformat_lib.Conf_t.t)
      -> ast_result

type result = string * string * ast_result option
type 'other f' = { f : 'ast. bool ref -> 'ast File_type.t -> 'ast -> 'ast * 'other }
type f = { f : 'ast. bool ref -> 'ast File_type.t -> 'ast -> 'ast }

let process_ast file_type structure (f : _ f') =
  let changed_something = ref false in
  let structure, res =
    f.f changed_something file_type
      (File_type.map file_type drop_concrete_syntax_constructs structure)
  in
  if !changed_something
  then
    Some
      ( structure
        |> File_type.map file_type undrop_concrete_syntax_constructs
        |> File_type.map file_type remove_attributes
      , res )
  else None

let process_file' ~fmconf:conf ~source_path ~input_name_matching_compilation_command
    (f : _ f') =
  let (T file_type) : File_type.packed =
    if
      false
      (* not yet ready, need to fix exception in fmast_diff and the fact that nothing
         works (reading .cmt instead of .cmti I think) *)
      && Build.is_mli source_path
    then T Intf
    else T Impl
  in
  let source_contents = In_channel.read_all (Cwdpath.to_string source_path) in
  let structure =
    Fmast.parse_with_ocamlformat ~conf
      ~input_name:
        (Option.value input_name_matching_compilation_command
           ~default:(Cwdpath.to_string source_path))
      (File_type.to_extended_ast file_type)
      source_contents
  in
  Fmast.update_structure structure (process_ast file_type __ f)
  |> Option.map ~f:(fun (structure', other) ->
         let source_contents' =
           Fmast.ocamlformat_print (File_type.to_extended_ast file_type) ~conf structure'
         in
         ( ( source_contents
           , source_contents'
           , Some (T (file_type, structure, structure', conf)) )
         , other ))

let process_file ~fmconf ~source_path ~input_name_matching_compilation_command (f : f) =
  process_file' ~fmconf ~source_path ~input_name_matching_compilation_command
    { f = (fun a b c -> (f.f a b c, ())) }
  |> Option.map ~f:(fun (a, ()) -> a)

module Requalify = struct
  let same_resolution ns (lid1, env1) (lid2, env2) =
    match Uast.find_by_name ns env1 (Conv.longident' lid1) with
    | exception Stdlib.Not_found ->
        if !log || debug.all
        then
          print_s
            [%sexp
              `same_resolution, `out_of_scope, (lid1 : Longident.t), (lid2 : Longident.t)];
        `Unknown
    | (path, _) as v -> (
        match Uast.find_by_name ns env2 (Conv.longident' lid2) with
        | v' ->
            if Shape.Uid.equal (Uast.uid ns v) (Uast.uid ns v')
            then (
              if !log || debug.all
              then
                print_s
                  [%sexp
                    `same_resolution, `same, (lid1 : Longident.t), (lid2 : Longident.t)];
              `Yes)
            else (
              if !log || debug.all
              then
                print_s
                  [%sexp
                    `same_resolution
                  , `differ
                  , ((lid1, Uast.uid ns v) : Longident.t * Uast.Shape.Uid.t)
                  , ((lid2, Uast.uid ns v') : Longident.t * Uast.Shape.Uid.t)];

              `No path)
        | exception Stdlib.Not_found ->
            if !log || debug.all
            then
              print_s
                [%sexp
                  `same_resolution
                , `differ
                , ((lid1, Uast.uid ns v) : Longident.t * Uast.Shape.Uid.t)
                , ((lid2, "Not_found") : Longident.t * string)];
            `No path)

  let rec ident_of_path_exn : Path.t -> Longident.t = function
    | Pident ident -> Lident (Ident.name ident)
    | Pdot (p, s) -> Ldot (ident_of_path_exn p, s)
    | Papply (p1, p2) -> Lapply (ident_of_path_exn p1, ident_of_path_exn p2)
    | Pextra_ty _ -> raise Stdlib.Not_found

  let rec idents_of_path : Path.t -> Fmast.Longident.t option list = function
    | Pident ident ->
        (* The file Foo might have a path Libname.Foo or Foo, depending on library
          wrapping, so we try to strip both Libname.Foo and Foo. *)
        if Ident.global ident
        then [ None; Some (Lident (Ident.name ident)) ]
        else [ Some (Lident (Ident.name ident)) ]
    | Pdot (p, s) ->
        List.map (idents_of_path p) ~f:(function
          | None -> Some (Fmast.Longident.Lident s)
          | Some lid -> Some (Ldot (lid, s)))
    | Papply (p1, p2) ->
        List.concat_map (idents_of_path p1) ~f:(function
          | None -> []
          | Some lid1 ->
              List.concat_map (idents_of_path p2) ~f:(function
                | None -> []
                | Some lid2 -> [ Some (Fmast.Longident.Lapply (lid1, lid2)) ]))
    | Pextra_ty _ -> []

  let idents_of_path path = List.filter_opt (idents_of_path path)

  let rec requalify : type a. (Path.t * a) Uast.ns -> _ -> _ -> Longident.t -> Longident.t
      =
   fun ns env1 env2 -> function
    | Lident s -> (
        match same_resolution ns (Lident s, env1) (Lident s, env2) with
        | `Unknown | `Yes -> Lident s
        | `No path -> ident_of_path_exn path)
    | Ldot (lid, s) -> Ldot (requalify Module env1 env2 lid, s)
    | Lapply (lid1, lid2) ->
        Lapply (requalify Module env1 env2 lid1, requalify ns env1 env2 lid2)

  let rec try_unqualifying_ident ~same_resolution_as_initially (env : Env.summary) var =
    let var =
      match env with
      | Env_open (_, path) -> (
          match
            List.find_map (idents_of_path path) ~f:(fun prefix ->
                let var = Flat_longident.from_longident var in
                let prefix = Flat_longident.from_longident prefix in
                Flat_longident.chop_prefix var ~prefix
                |> Option.map ~f:Flat_longident.to_longident)
          with
          | Some var' when same_resolution_as_initially var' -> var'
          | _ -> var)
      | _ -> var
    in
    match Uast.Env_summary.next env with
    | None -> var
    | Some env -> try_unqualifying_ident ~same_resolution_as_initially env var
end
