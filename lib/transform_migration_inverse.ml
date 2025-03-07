open Base
open! Common
open! Ocamlformat_ocaml_common
open Ocamlformat_parser_extended
module P = Parsetree
open Fmast
open! Transform_common

let join_used_vars = function
  | [] -> Map.empty (module String)
  | _ :: _ as l ->
      List.reduce_exn l ~f:(fun map1 map2 ->
          Map.merge_skewed map1 map2 ~combine:(fun ~key:var _loc1 loc2 ->
              Location.raise_errorf ~loc:loc2 "multiple use of variable %s" var))

(* The idea is that to inverse
   List.map [@@migrate { repl = fun f l -> Base.List.map l ~f }],
   we start from the equation:
   - List.map = fun f l -> Base.List.map l ~f
   - List.map f l = (fun f l -> Base.List.map l ~f) f l
                  = Base.List.map l ~f
     because if e1 = e2, then for forall f l. e1 f l = e2 f l
   - (fun l ~f -> List.map f l) = (fun l ~f -> Base.List.map l ~f)
                                = Base.List.map
     because if forall f l. e1 = e2, then (fun l ~f -> e1) = (fun l ~f -> e2)
   Thus Base.List.map [@migrate { repl = (fun l ~f -> List.map f l) }].

   We're probably not doing enough checks to ensure that the function we're generating
   is correct.

   For instance, if we start with the equation f = (fun a b -> g a), we'd generate (fun
   a -> f a b) = g.

   The dropping of used vars is also suspicious.

   Also, the set of annotations might not be injective (for instance List.filter and
   List.find_all become Base.List.filter), and the duplicate annotation will result in
   whichever annotation is visited last to override the other ones.

   For cases where the replacement function is more general than the initial function,
   like List.mem -> Base.List.mem ~equal:Base.phys_equal, we can still support it,
   provided we have support for context matching:
   function [%context it ~equal:Base.phys_equal] -> List.mem
   In fact, we can always inverse a migration this way, but to make it more generally
   applicable we want to reduce the context, which is effectively what inverse_equation
   does.
*)
let rec inverse_equation (expr1 : P.expression) (expr2 : P.expression) =
  match expr2.pexp_desc with
  | Pexp_ident ident -> (expr1, ident)
  | Pexp_function (params2, None, Pfunction_body body2) ->
      let args1 = List.map params2 ~f:inverse_function_param in
      inverse_equation { expr2 with pexp_desc = Pexp_apply (expr1, args1) } body2
  | Pexp_apply (fun_, args) ->
      let params1, used = List.map args ~f:inverse_function_arg |> List.unzip in
      ignore (join_used_vars used) (* ?? *);
      inverse_equation
        { expr2 with pexp_desc = Pexp_function (params1, None, Pfunction_body expr1) }
        fun_
  | Pexp_match (scrutinee, cases) ->
      let cases2 = inverse_cases cases in
      inverse_equation { expr2 with pexp_desc = Pexp_match (expr1, cases2) } scrutinee
  | _ -> Location.raise_errorf ~loc:expr2.pexp_loc "unsupported expression"

and inverse_cases (cases : P.case list) =
  (* If the initial cases are not surjective, the resulting pattern match would have
     exhaustivity warnings, which we don't check. If the initial cases are not injective,
     the resulting pattern match would have redundant cases, which we don't check either.
  *)
  List.map cases ~f:inverse_case

and inverse_case ({ pc_lhs; pc_guard; pc_rhs } : P.case) =
  Option.iter pc_guard ~f:(fun g ->
      Location.raise_errorf ~loc:g.pexp_loc "pattern match guards are unsupported");
  let expr = inverse_pat pc_lhs in
  let pat, _used = inverse_expr pc_rhs in
  { pc_lhs = pat; pc_guard = None; pc_rhs = expr }

and inverse_function_param (p : P.expr_function_param) : Asttypes.arg_label * P.expression
    =
  match p.pparam_desc with
  | Pparam_newtype _ ->
      Location.raise_errorf ~loc:p.pparam_loc "fun (type ..) -> .. is not supported"
  | Pparam_val (arg_label, default, pat) -> (
      match default with
      | Some e -> Location.raise_errorf ~loc:e.pexp_loc "default parameters not supported"
      | None -> (arg_label, inverse_pat pat))

and inverse_function_arg (arg_label, e) : P.expr_function_param * _ =
  let p, used = inverse_expr e in
  ({ pparam_desc = Pparam_val (arg_label, None, p); pparam_loc = e.pexp_loc }, used)

and inverse_expr (e : P.expression) : P.pattern * Location.t Map.M(String).t =
  match e.pexp_desc with
  | Pexp_ident { txt = Lident var; loc } ->
      ( Ast_helper.Pat.var ~loc:e.pexp_loc ~attrs:e.pexp_attributes { txt = var; loc }
      , Map.singleton (module String) var loc )
  | Pexp_tuple l ->
      let ps, used = List.unzip (List.map l ~f:inverse_expr) in
      ( Ast_helper.Pat.tuple ~loc:e.pexp_loc ~attrs:e.pexp_attributes ps
      , join_used_vars used )
  | Pexp_construct (id, e_opt) ->
      let p_opt, used =
        match e_opt with
        | None -> (None, Map.empty (module String))
        | Some e ->
            let p', used = inverse_expr e in
            (Some ([], p'), used)
      in
      (Ast_helper.Pat.construct ~loc:e.pexp_loc ~attrs:e.pexp_attributes id p_opt, used)
  (* We'd need to support Pexp_fun/Pexp_apply here, to support cases like
     List.partition_map. Perhaps this could be done by generalizing inverse_equation
     from: expr1 -> expr2 -> new_expr1, ident2 into
     expr2 -> (fun ident1 -> expr1) * ident2
     so we don't have to know upfront what variable we're solving for. *)
  | _ -> Location.raise_errorf ~loc:e.pexp_loc "unsupported expression"

and inverse_pat (p : P.pattern) : P.expression =
  match p.ppat_desc with
  | Ppat_var var ->
      Ast_helper.Exp.ident ~loc:p.ppat_loc ~attrs:p.ppat_attributes
        { txt = Lident var.txt; loc = var.loc }
  | Ppat_tuple l ->
      Ast_helper.Exp.tuple ~loc:p.ppat_loc ~attrs:p.ppat_attributes
        (List.map l ~f:inverse_pat)
  | Ppat_construct (id, p_opt) ->
      let e_opt =
        Option.map
          ~f:(function
            | [], p -> inverse_pat p
            | _ -> Location.raise_errorf ~loc:p.ppat_loc "unsupported pattern")
          p_opt
      in
      Ast_helper.Exp.construct ~loc:p.ppat_loc ~attrs:p.ppat_attributes id e_opt
  | _ -> Location.raise_errorf ~loc:p.ppat_loc "unsupported pattern"

let run_structure file_type structure =
  let changed_something = ref false in
  (* this code doesn't care, but make a dummy value to hand out to other functions *)
  let super = Ast_mapper.default_mapper in
  let self =
    { super with
      expr =
        (fun self expr ->
          match Transform_migration.find_extra_migration_fmast expr with
          | None -> super.expr self expr
          | Some (src, src_id, _, { repl; libraries = _ }) -> (
              match inverse_equation { src with pexp_attributes = [] } repl with
              | exception e ->
                  if not in_test
                  then
                    print_s [%sexp "can't inverse", (src_id.txt : Longident.t), (e : exn)];
                  Ast_helper.with_default_loc repl.pexp_loc (fun () ->
                      Ast_helper.Exp.string
                        ("can't inverse "
                        ^ String.concat ~sep:"." (Longident.flatten src_id.txt)))
              | repl', id' ->
                  (* Ideally, we'd check the absolute path to [id] in the typed tree,
                     and used that in the replacement code. *)
                  { expr with
                    pexp_desc = Pexp_ident id'
                  ; pexp_attributes =
                      Transform_migration.update_attribute_payload_fmast
                        expr.pexp_attributes
                        { repl = Some repl'; libraries = [] }
                  }))
    ; structure_item =
        update_migrate_test_payload
          ~match_attr:(__ =: "migrate_test.inverse")
          super ~changed_something
        |> __.next
    }
  in
  structure |> File_type.map file_type self |> File_type.map file_type remove_attributes

let run ~fmconf ~source_path =
  process_file ~fmconf ~source_path
    { f =
        (fun changed_something file_type structure ->
          changed_something := true;
          run_structure file_type structure)
    }
