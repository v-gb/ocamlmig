open Base
open! Common
open! Ocamlformat_ocaml_common
open Ocamlformat_parser_extended
module P = Parsetree
open Fmast
open! Transform_common

let run ~fmconf ~source_path =
  process_file ~fmconf ~source_path (fun changed_something structure ->
      let super = Ast_mapper.default_mapper in
      let self =
        { super with
          expr =
            (fun self expr ->
              match Transform_migration.find_extra_migration_fmast expr with
              | Some (id_expr, _, src_syntax, ({ repl; _ } as payload))
                when (* ideally, we'd exclude use of Rel in replacement as well *)
                     (not (Transform_migration.has_context_match repl))
                     && match src_syntax with `Id -> true | `Structure -> false ->
                  changed_something := true;
                  { expr with
                    pexp_desc =
                      (Ast_helper.Exp.list
                         [ { id_expr with
                             pexp_attributes =
                               Transform_migration.remove_attribute_payload_fmast
                                 id_expr.pexp_attributes
                           }
                         ; repl
                         ])
                        .pexp_desc
                  ; pexp_attributes =
                      Transform_migration.update_attribute_payload_fmast
                        expr.pexp_attributes { payload with repl = None }
                  }
              | _ -> super.expr self expr)
        ; structure_item = update_migrate_test_payload super ~changed_something
        }
      in
      self.structure self structure |> call remove_attributes)
