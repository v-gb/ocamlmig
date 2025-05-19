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

let type_extra_migration ~type_index ~env (e1 : P.expression) e2 =
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

let type_sigi ~env ~loc (tty : Typedtree.core_type option) e =
  match tty with
  | None ->
      ignore (Typecore.type_expression env (uexpr_of_fmexpr { e with pexp_loc = loc }))
  | Some tty ->
      let tty = Ctype.instance tty.ctyp_type in
      ignore
        (Typecore.type_argument env (uexpr_of_fmexpr { e with pexp_loc = loc }) tty tty)

let report_many_exns exns =
  List.iter exns ~f:(fun e ->
      match Uast.Location.report_exception Format.err_formatter e with
      | () -> ()
      | exception _ -> (
          match Fmast.Location.report_exception Format.err_formatter e with
          | () -> ()
          | exception _ -> raise e));
  raise Location.Already_displayed_error

let find_migration_sigi_fmast ~type_index (sigi : signature_item) =
  match sigi.psig_desc with
  | Psig_value val_desc ->
      Transform_migration.find_attribute_payload_fmast
        (val_desc.pval_attributes.attrs_before @ val_desc.pval_attributes.attrs_after)
      |> Option.map ~f:(fun migration ->
             ( migration
             , val_desc.pval_loc
             , Build.Type_index.find (force type_index) Typ val_desc.pval_type |> List.hd
             ))
  | _ -> None

let run_structure (type a) changed_something (file_type : a File_type.t) (structure : a)
    ~type_index ~add_to_load_path =
  let libs =
    set_from_iter
      (module String)
      (fun yield ->
        let super = Ast_mapper.default_mapper in
        let self =
          { super with
            expr =
              (fun self v ->
                (match Transform_migration.find_extra_migration_fmast v with
                | None -> ()
                | Some (_, _, _, { libraries; _ }) ->
                    ignore (force type_index);
                    List.iter libraries ~f:yield);
                super.expr self v)
          ; signature_item =
              (fun self v ->
                (match find_migration_sigi_fmast ~type_index v with
                | None -> ()
                | Some ({ libraries; _ }, _, _) -> List.iter libraries ~f:yield);
                super.signature_item self v)
          }
        in
        ignore (structure |> File_type.map file_type self))
  in
  add_to_load_path libs;
  let super = Ast_mapper.default_mapper in
  let errors = ref [] in
  let self, self_signature =
    (* We should handle let and Rel *)
    let env = lazy (Compmisc.initial_env ()) in
    let ast_of_errors ~loc errors =
      let loc =
        update_loc { loc with loc_ghost = true } (fun pos ->
            { pos with pos_fname = migrate_filename `Gen })
      in
      let expr_of_exn e =
        Ast_helper.Exp.string ~loc
          (String.strip
             (try Format.asprintf "%a" Uast.Location.report_exception e
              with _ -> (
                try Format.asprintf "%a" Fmast.Location.report_exception e
                with _ -> Exn.to_string e)))
      in
      Ast_helper.Str.eval ~loc
        (match errors with
        | [] -> Ast_helper.Exp.string ~loc "types"
        | _ -> Ast_helper.Exp.tuple ~loc (List.map errors ~f:expr_of_exn))
    in
    let signature_item ?mty_type self v =
      let next () =
        (match find_migration_sigi_fmast ~type_index v with
        | None -> ()
        | Some ({ repl; libraries = _ }, loc, ttyp) -> (
            let env = force env in
            let env =
              match mty_type with
              | None | Some (lazy None) -> env
              | Some (lazy (Some (mty_type : Types.module_type))) ->
                  Env.add_module (Ident.create_local "Rel") Mp_present mty_type env
            in
            try ignore (type_sigi ~env ~loc ttyp repl) with e -> errors := e :: !errors));
        super.signature_item self v
      in
      match update_migrate_test signature_item v with
      | None -> next ()
      | Some (attr, rebuild) ->
          if not in_test
          then v
          else (
            changed_something := true;
            let errors =
              Ref.set_temporarily errors [] ~f:(fun () ->
                  ignore (next ());
                  List.rev !errors)
            in
            rebuild
              (Some
                 { attr with
                   attr_payload = PStr [ ast_of_errors ~loc:Location.none errors ]
                 }))
    in
    let signature ?mty_type self l = List.map l ~f:(signature_item ?mty_type self) in
    ( { super with
        expr =
          with_log (fun self expr ->
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
                         try ignore (type_extra_migration ~env ~type_index e repl)
                         with e -> errors := e :: !errors));
              super.expr self expr)
      ; structure_item =
          (fun self v ->
            let next () = super.structure_item self v in
            match update_migrate_test structure_item v with
            | None -> next ()
            | Some (attr, rebuild) ->
                if not in_test
                then v
                else (
                  changed_something := true;
                  let errors =
                    Ref.set_temporarily errors [] ~f:(fun () ->
                        ignore (next ());
                        List.rev !errors)
                  in
                  rebuild
                    (Some
                       { attr with
                         attr_payload = PStr [ ast_of_errors ~loc:Location.none errors ]
                       })))
      ; module_type =
          (fun self v ->
            match v.pmty_desc with
            | Pmty_signature l ->
                let mty_type =
                  lazy
                    (match Build.Type_index.find (force type_index) Mtyp v with
                    | [] -> None
                    | tmty :: _ -> Some tmty.mty_type)
                in
                { v with pmty_desc = Pmty_signature (signature ~mty_type self l) }
            | _ -> super.module_type self v)
      ; signature_item
      }
    , signature )
  in
  let structure' =
    match file_type with
    | Impl -> File_type.map file_type self structure
    | Intf ->
        self_signature
          ~mty_type:
            (lazy
              (match Build.Type_index.overall (force type_index) with
              | Some (`Signature s) -> Some (Mty_signature s.sig_type)
              | None | Some (`Structure _) -> assert false))
          self structure
  in
  if not (List.is_empty !errors) then report_many_exns (List.rev !errors);
  structure' |> File_type.map file_type remove_attributes

let run ~fmconf ~source_path ~add_to_load_path ~type_index =
  process_file ~fmconf ~source_path { f = run_structure ~add_to_load_path ~type_index }
