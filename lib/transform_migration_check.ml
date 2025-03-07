open Base
open! Common
open! Ocamlformat_ocaml_common
open Ocamlformat_parser_extended
module P = Parsetree
open Fmast
open! Transform_common

let exprs_and_repls e repl =
  if Transform_migration.has_context_match repl
  then
    match repl.pexp_desc with
    | Pexp_function ([], None, Pfunction_cases (cases, _, _)) ->
        List.concat_map cases ~f:(fun case ->
            match case.pc_lhs with
            | { ppat_desc = Ppat_extension ({ txt = "context"; _ }, PTyp user_typ); _ } ->
                [ (Ast_helper.Exp.constraint_ e user_typ, case.pc_rhs) ]
            | { ppat_desc = Ppat_any; _ } -> [ (e, case.pc_rhs) ]
            | _ -> [])
    | _ -> assert false
  else [ (e, repl) ]

let run_structure changed_something file_type structure =
  let super = Ast_mapper.default_mapper in
  let self =
    { super with
      expr =
        (fun self expr ->
          match Transform_migration.find_extra_migration_fmast expr with
          | None -> super.expr self expr
          | Some (id_expr, _, _, { repl; libraries = _ }) ->
              changed_something := true;
              exprs_and_repls
                { id_expr with
                  pexp_attributes =
                    Transform_migration.remove_attribute_payload_fmast
                      id_expr.pexp_attributes
                }
                repl
              |> List.map ~f:(fun (e, repl) ->
                     (* a list forces the two types to unify, while allowing extra
                        optional arguments in the second type to be omitted *)
                     Ast_helper.Exp.list [ e; repl ])
              |> List.rev
              |> List.reduce ~f:(fun acc e ->
                     Ast_helper.Exp.let'
                       [ Ast_helper.Vb.mk (Ast_helper.Pat.any ()) [] (Pfunction_body e)
                           ~is_pun:false
                       ]
                       ~loc_in:Location.none acc)
              |> Option.value ~default:(Ast_helper.Exp.unit ()))
    ; structure_item = update_migrate_test_payload super ~changed_something |> __.next
    }
  in
  structure |> File_type.map file_type self |> File_type.map file_type remove_attributes

let run ~fmconf ~source_path = process_file ~fmconf ~source_path { f = run_structure }
