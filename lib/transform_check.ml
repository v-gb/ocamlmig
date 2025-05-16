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

let type_ ~type_index ~env (e1 : P.expression) e2 =
  match
    match Build.Type_index.find (force type_index) Exp e1 with
    | [] -> None
    | texp1 :: _ -> Some (Ctype.instance texp1.exp_type)
  with
  | None ->
      ignore
        (Typecore.type_expression env
           (uexpr_of_fmexpr { e2 with pexp_loc = e1.pexp_loc }))
  | Some t1 ->
      (* We propagate the type of t1 into the typing of t2 so that extra optional
         arguments in the type of t2 would be erased by the typer instead of causing
         unification errors. type_expected is not enough to do this, as it doesn't erase
         arguments. *)
      ignore
        (Typecore.type_argument env
           (uexpr_of_fmexpr { e2 with pexp_loc = e1.pexp_loc })
           t1 t1)

let report_many_exns exns =
  List.iter exns ~f:(fun e ->
      match Uast.Location.report_exception Format.err_formatter e with
      | () -> ()
      | exception _ -> (
          match Fmast.Location.report_exception Format.err_formatter e with
          | () -> ()
          | exception _ -> raise e));
  raise Location.Already_displayed_error

let run_structure changed_something file_type structure ~type_index ~add_to_load_path =
  let libs =
    set_from_iter
      (module String)
      (fun yield ->
        let super = Ast_mapper.default_mapper in
        let self =
          { super with
            expr =
              (fun self expr ->
                (match Transform_migration.find_extra_migration_fmast expr with
                | None -> ()
                | Some (_, _, _, { libraries; _ }) ->
                    ignore (force type_index);
                    List.iter libraries ~f:yield);
                super.expr self expr)
          }
        in
        ignore (structure |> File_type.map file_type self))
  in
  add_to_load_path libs;
  let super = Ast_mapper.default_mapper in
  let errors = ref [] in
  let self =
    (* We should handle val and Rel *)
    let env = lazy (Compmisc.initial_env ()) in
    { super with
      expr =
        (fun self expr ->
          (match Transform_migration.find_extra_migration_fmast expr with
          | None -> ()
          | Some (id_expr, _, _, { repl; libraries = _ }) ->
              exprs_and_repls
                { id_expr with
                  pexp_attributes =
                    Transform_migration.remove_attribute_payload_fmast
                      id_expr.pexp_attributes
                }
                repl
              |> List.iter ~f:(fun (e, repl) ->
                     let env = force env in
                     try ignore (type_ ~env ~type_index e repl)
                     with e -> errors := e :: !errors));
          super.expr self expr)
    ; structure_item =
        (let state = ref false in
         update_migrate_test_payload
           { super with
             structure_item =
               (fun self si ->
                 if not !state
                 then super.structure_item self si
                 else
                   let () = () in
                   if not in_test
                   then si
                   else
                     let errors =
                       Ref.set_temporarily errors [] ~f:(fun () ->
                           ignore (super.structure_item self si);
                           List.rev !errors)
                     in
                     let expr_of_exn e =
                       Ast_helper.Exp.string
                         (String.strip
                            (try Format.asprintf "%a" Uast.Location.report_exception e
                             with _ -> (
                               try Format.asprintf "%a" Fmast.Location.report_exception e
                               with _ -> Exn.to_string e)))
                     in
                     Ast_helper.Str.eval
                       (match errors with
                       | [] -> Ast_helper.Exp.string "types"
                       | _ -> Ast_helper.Exp.tuple (List.map errors ~f:expr_of_exn)))
           }
           ~state ~changed_something
         |> __.next)
    }
  in
  let structure' = structure |> File_type.map file_type self in
  if not (List.is_empty !errors) then report_many_exns (List.rev !errors);
  structure' |> File_type.map file_type remove_attributes

let run ~fmconf ~source_path ~add_to_load_path ~type_index =
  process_file ~fmconf ~source_path { f = run_structure ~add_to_load_path ~type_index }
