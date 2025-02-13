open! Base
open! Stdio
open! Common

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

let drop_concrete_syntax_constructs method_ v =
  let super = Ast_mapper.default_mapper in
  let self =
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
              { expr with
                pexp_desc = Pexp_apply (fun_, [ (Nolabel, e1); (Nolabel, e2) ])
              }
          | _ -> expr)
    ; value_binding =
        (fun self binding ->
          let binding = super.value_binding self binding in
          match (binding.pvb_args, binding.pvb_body) with
          | [], Pfunction_body _ -> binding
          | (_ :: _ as params), body | params, (Pfunction_cases _ as body) ->
              (* The ocamlformat ast is confusing/confused here.

                In principle, anytime you have a [fun .. -> ..], a type annotation
                before the arrow and a [function ..] in the body all get combined
                to make a single Pexp_function node, whose most general syntax is
                [fun .. : int -> function .. -> ..]. With two invariants:
                - the function must take at least one argument (so either a param, or
                  Pfunction_cases)
                - there can't be a type annotation if the param list is empty

                Separately, ocamlformat wants to distinguish, in the ast, between
                [let f x = ...] and [let f = fun x -> ..].

                By combining these two things, it parses [let f : int = function ..]
                into pvb_params = [];
                     pvb_constraint = Some "int"
                     pvb_body = Pfunction_case ...

                which is neither a simple let-binding (because of Pfunction_cases)
                and is not an "inlined" pexp_function either (because it breaks the
                second invariant).
                
                The correct ast would be:
                     pvb_params = [];
                     pvb_constraint = Some "int"
                     pvb_body = Pfunction_body (Pexp_function ([], None, Pfunction_cases ...))

                which is what happens if you add parens around the (function ..) above.

                The bad case happens when the params list is empty here. So in that
                case, we separate the type annotation (if any) from the function cases.
               *)
              let leave_type_constraint_alone = List.is_empty params in
              let body =
                let loc = binding.pvb_loc in
                Ast_helper.Exp.function_ ~loc params
                  (if leave_type_constraint_alone
                   then None
                   else
                     Option.map binding.pvb_constraint
                       ~f:Fmast.type_constraint_of_value_constraint)
                  body
                  ~attrs:[ Sattr.pun.build ~loc:!Ast_helper.default_loc () ]
              in
              { binding with
                pvb_args = []
              ; pvb_body = Pfunction_body body
              ; pvb_constraint =
                  (if leave_type_constraint_alone then binding.pvb_constraint else None)
              })
    }
  in
  (method_ self) self v

let undrop_concrete_syntax_constructs method_ v =
  let super = Ast_mapper.default_mapper in
  let self =
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
  in
  (method_ self) self v

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

let call (class_ : Ast_mapper.mapper) v = class_.structure class_ v

let process_ast structure f =
  let changed_something = ref false in
  let structure, res =
    f changed_something (drop_concrete_syntax_constructs __.structure structure)
  in
  if !changed_something
  then
    Some
      ( structure
        |> undrop_concrete_syntax_constructs __.structure
        |> call remove_attributes
      , res )
  else None

type result =
  string
  * string
  * (Ocamlformat_lib.Extended_ast.structure
     Ocamlformat_lib.Parse_with_comments.with_comments
    * Ocamlformat_lib.Extended_ast.structure
      Ocamlformat_lib.Parse_with_comments.with_comments
    * Ocamlformat_lib.Conf_t.t)
    option

let process_file' ~fmconf:conf ~source_path ~input_name_matching_compilation_command f =
  let source_contents = In_channel.read_all (Cwdpath.to_string source_path) in
  let structure =
    Fmast.parse_with_ocamlformat ~conf
      ~input_name:
        (Option.value input_name_matching_compilation_command
           ~default:(Cwdpath.to_string source_path))
      Structure source_contents
  in
  Fmast.update_structure structure (process_ast __ f)
  |> Option.map ~f:(fun (structure', other) ->
         let source_contents' = Fmast.ocamlformat_print Structure ~conf structure' in
         ( ((source_contents, source_contents', Some (structure, structure', conf))
             : result)
         , other ))

let process_file ~fmconf ~source_path ~input_name_matching_compilation_command f =
  process_file' ~fmconf ~source_path ~input_name_matching_compilation_command (fun a b ->
      (f a b, ()))
  |> Option.map ~f:(fun (a, ()) -> a)
