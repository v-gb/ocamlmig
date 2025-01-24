open Base
open Common
open Transform_common

let may_need_type_index pattern =
  let super = Ast_mapper.default_mapper in
  let self =
    { super with
      expr =
        (fun self expr ->
          (match expr.pexp_desc with
          | Pexp_constraint _ -> Exn.raise_without_backtrace Stdlib.Exit
          | _ -> ());
          super.expr self expr)
    }
  in
  try
    ignore (self.expr self pattern);
    false
  with Stdlib.Exit -> true

open! Ocamlformat_ocaml_common
open Ocamlformat_parser_extended
module P = Parsetree
open! Fmast

(* Possible design:
   - __etc to match any number of tuple elements/list elements
     Or maybe it could be __foo*, with a simple rewriting of such tokens.
   - it may be better for pexp_ident to match the value rather than the syntactically,
     and have some mechanism for a syntactic match. Maybe [%exact foo]?
   - might be useful to have ways of stating some properties of the code like
     "cannot raise" or "duplicatable". Perhaps we could support general conjunction
     and disjunction with (e1 & e2) and (e1 or e2), such that you can say things like
     (try Some (Sys.getenv (__var & [%noraise])) with Not_found -> None) /// Sys.getenv_opt __var
*)

type value =
  | Expr of P.expression
  | Args of function_arg list

type stage2 =
     Parsetree.expression
  -> env:value Map.M(String).t ref
  -> ctx:Build.Type_index.t option Lazy.t
  -> bool

let match_option match_x x_opt =
  match x_opt with
  | None -> fun y_opt ~env:_ ~ctx:_ -> Option.is_none y_opt
  | Some x -> (
      let s = match_x x in
      fun y_opt ~env ~ctx -> match y_opt with None -> false | Some y -> s y ~env ~ctx)

let match_list match_x xs f =
  let stage2s = List.map xs ~f:match_x in
  fun y ~env ~ctx ->
    match f y with
    | None -> false
    | Some ys -> (
        match List.for_all2 stage2s ys ~f:(fun stage2 y -> stage2 y ~env ~ctx) with
        | Unequal_lengths -> false
        | Ok b -> b)

let match_var v_motif make_data =
  if v_motif =: "__" (* has a dedicated branch to support multiple __ patterns *)
  then fun _ ~env:_ ~ctx:_ -> true
  else
    fun data ~env ~ctx:_ ->
      match Map.add !env ~key:v_motif ~data:(make_data data) with
      | `Ok map ->
          env := map;
          true
      | `Duplicate -> false

let etc_var : function_arg -> _ = function
  | Nolabel, { pexp_desc = Pexp_ident { txt = Lident v; _ }; _ } ->
      if String.is_prefix v ~prefix:"__etc" then Some v else None
  | _ -> None

let unsupported_motif loc =
  Location.raise_errorf ~loc:(Conv.location loc) "unsupported motif syntax"

let rec match_ (motif : Uast.Parsetree.expression) : stage2 =
  match motif.pexp_desc with
  | Pexp_ident { txt = Lident v; _ } when String.is_prefix v ~prefix:"__" ->
      match_var v (fun e -> Expr e)
  | Pexp_constraint (m1, typ) -> (
      let user_type = Uast.type_type typ in
      let stage1 = match_ m1 in
      fun expr ~env ~ctx ->
        match Lazy.force ctx with
        | None ->
            if !log then print_s [%sexp "missing type index"];
            false
        | Some index -> (
            match Build.Type_index.expr index (Conv.location' expr.pexp_loc) with
            | texpr :: _ ->
                (let does_match =
                   Uast.match_typ ~env:texpr.exp_env texpr.exp_type ~user_type
                 in
                 if !log
                 then
                   print_s
                     [%sexp
                       "context"
                     , (Format.asprintf "%a" Printtyp.type_expr texpr.exp_type : string)
                     , "vs"
                     , (Format.asprintf "%a" Printtyp.type_expr user_type : string)
                     , ~~(does_match : bool)];
                 does_match)
                && stage1 expr ~env ~ctx
            | [] ->
                if !log then print_s [%sexp "no type"];
                false))
  | Pexp_ident id_motif -> (
      fun expr ~env:_ ~ctx:_ ->
        match expr.pexp_desc with
        | Pexp_ident id ->
            Fmast.Longident.compare id.txt (Conv.longident id_motif.txt) = 0
        | _ -> false)
  | Pexp_tuple motifs ->
      match_list match_ motifs (function
        | ({ pexp_desc = Pexp_tuple es; _ } : P.expression) -> Some es
        | _ -> None)
  | Pexp_apply
      ( { pexp_desc = Pexp_ident { txt = Lident (("&" | "or") as op); _ }; _ }
      , [ (Nolabel, m1); (Nolabel, m2) ] ) -> (
      let s1 = match_ m1 in
      let s2 = match_ m2 in
      match op with
      | "&" -> fun expr ~env ~ctx -> s1 expr ~env ~ctx && s2 expr ~env ~ctx
      | "or" ->
          fun expr ~env ~ctx ->
            (* This kind of eager matching may not be enough? *)
            let env_before = !env in
            s1 expr ~env ~ctx
            ||
            (env := env_before;
             s2 expr ~env ~ctx)
      | _ -> assert false)
  | Pexp_apply (mf, margs) -> (
      let sf = match_ mf in
      let sargs = match_args margs in
      fun expr ~env ~ctx ->
        match expr.pexp_desc with
        | Pexp_apply (f, args) -> sf f ~env ~ctx && sargs args ~env ~ctx
        | _ -> false)
  | Pexp_construct (id_motif, motif_opt) -> (
      let sopt = match_option match_ motif_opt in
      fun expr ~env ~ctx ->
        match expr.pexp_desc with
        | Pexp_construct (id, eopt) ->
            Fmast.Longident.compare id.txt (Conv.longident id_motif.txt) = 0
            && sopt eopt ~env ~ctx
        | _ -> false)
  | Pexp_extension (motif_name, motif_payload) -> (
      let s_payload = match_payload ~loc:motif.pexp_loc motif_payload in
      fun expr ~env ~ctx ->
        match expr.pexp_desc with
        | Pexp_extension (name, payload) ->
            name.txt =: motif_name.txt && s_payload payload ~env ~ctx
        | _ -> false)
  | _ -> unsupported_motif motif.pexp_loc

and match_args (margs : (Uast.Asttypes.arg_label * Uast.Parsetree.expression) list) =
  let var_other = ref None in
  let s_named = ref (Map.empty (module String)) in
  let s_anon = Queue.create () in
  List.iter margs ~f:(fun (arg_label, motif) ->
      match arg_label with
      | Nolabel -> (
          match motif.pexp_desc with
          | Pexp_ident { txt = Lident v; _ } when String.is_prefix v ~prefix:"__etc" ->
              var_other := Some v
          | _ -> Queue.enqueue s_anon (match_ motif))
      | _ ->
          s_named :=
            Map.add_exn !s_named
              ~key:(Arg_label.to_string (Conv.arg_label arg_label))
              ~data:(Conv.arg_label arg_label, match_ motif));
  let var_other = !var_other in
  let s_named = !s_named in
  let s_anon = Queue.to_list s_anon in
  fun args ~env ~ctx ->
    let s_anon = ref s_anon in
    let others = ref [] in
    let match_others arg_label arg =
      if Option.is_some var_other
      then (
        others := (arg_label, arg) :: !others;
        true)
      else false
    in
    let matched =
      List.for_all args ~f:(fun (arg_label, arg) ->
          match arg_label with
          | Nolabel -> (
              match !s_anon with
              | stage2 :: rest ->
                  s_anon := rest;
                  stage2 arg ~env ~ctx
              | [] -> match_others arg_label arg)
          | _ -> (
              match Map.find s_named (Fmast.Arg_label.to_string arg_label) with
              | Some (motif_arg_label, stage2) ->
                  (* label comparison probably not ideal, for optional arguments? *)
                  Arg_label.equal motif_arg_label arg_label && stage2 arg ~env ~ctx
              | None -> match_others arg_label arg))
    in
    matched
    &&
    match var_other with
    | None -> true
    | Some var_other -> (
        match Map.add !env ~key:var_other ~data:(Args (List.rev !others)) with
        | `Ok map ->
            env := map;
            true
        | `Duplicate -> false)

and match_payload ~loc (motif : Uast.Parsetree.payload) =
  match motif with
  | PStr m -> (
      let sm = match_structure ~loc m in
      fun s ~env ~ctx -> match s with PStr si -> sm si ~env ~ctx | _ -> false)
  | _ -> unsupported_motif loc

and match_structure ~loc (stritems : Uast.Parsetree.structure_item list) =
  match_list (match_structure_item ~loc) stritems Option.some

and match_structure_item ~loc (motif : Uast.Parsetree.structure_item) =
  match motif.pstr_desc with
  | Pstr_eval (motif1, _) -> (
      let s1 = match_ motif1 in
      fun (s : P.structure_item) ~env ~ctx ->
        match s.pstr_desc with Pstr_eval (e, _) -> s1 e ~env ~ctx | _ -> false)
  | _ -> unsupported_motif loc

let subst expr ~env =
  let super = Ast_mapper.default_mapper in
  let self =
    { super with
      expr =
        with_log (fun self expr ->
            let expr =
              match expr.pexp_desc with
              | Pexp_apply (f, args) when Option.is_some (List.find_map args ~f:etc_var)
                ->
                  let f' = self.expr self f in
                  let args' =
                    List.concat_map args ~f:(fun (arg_label, arg) ->
                        match etc_var (arg_label, arg) with
                        | Some v -> (
                            match Map.find_exn env v with
                            | Expr _ -> Location.raise_errorf ~loc:arg.pexp_loc "hm, what"
                            | Args args -> args)
                        | None -> [ (arg_label, self.expr self arg) ])
                  in
                  { expr with pexp_desc = Pexp_apply (f', args') }
              | _ -> super.expr self expr
            in
            let expr = super.expr self expr in
            match expr.pexp_desc with
            | Pexp_apply (f, args)
              when not (Attr.exists expr.pexp_attributes (Attr.reorder `Internal)) ->
                let args' = Transform_migration.commute_args args in
                { expr with pexp_desc = Pexp_apply (f, args') }
            | Pexp_ident { txt = Lident var; _ } when Map.mem env var -> (
                match Map.find_exn env var with
                | Expr e -> e
                | Args _ ->
                    Location.raise_errorf ~loc:expr.pexp_loc
                      "pattern %s can only be inserted into a function call" var)
            | _ -> expr)
    }
  in
  self.expr self expr

let replace expr ~type_index ~stage2 ~repl =
  let env = ref (Map.empty (module String)) in
  if stage2 expr ~env ~ctx:type_index
  then
    let repl = preserve_loc_to_preserve_comment_pos ~from:expr repl in
    Some (subst repl ~env:!env)
  else None

let run motif_and_repls () =
  let may_need_type_index_ref = ref false in
  let stage2_and_repls =
    List.map motif_and_repls ~f:(fun (motif, repl) ->
        let motif =
          Uast.Parse.expression (lexing_from_string motif ~file_path:"command line param")
        in
        if may_need_type_index motif then may_need_type_index_ref := true;
        let repl = ref (`Unforced repl) in
        (match_ motif, repl))
  in
  fun ~fmconf ~type_index ~source_path ~input_name_matching_compilation_command ->
    let stage2_and_repls =
      List.map stage2_and_repls ~f:(fun (stage2, repl) ->
          let repl =
            match !repl with
            | `Forced v -> v
            | `Unforced repl_str ->
                let v =
                  (Fmast.parse_with_ocamlformat Expression ~conf:fmconf
                     ~input_name:migrate_filename_gen
                     (* important for comment placement, among which
                        preserve_loc_to_preserve_comment_pos to work *)
                     repl_str)
                    .ast
                  |> Transform_migration.internalize_reorder_attribute
                in
                repl := `Forced v;
                v
          in
          (stage2, repl))
    in
    let input_name_matching_compilation_command =
      if !may_need_type_index_ref
      then force input_name_matching_compilation_command
      else None
    in
    process_file ~fmconf ~source_path ~input_name_matching_compilation_command
      (fun changed_something structure ->
        let super = Ast_mapper.default_mapper in
        let self =
          { super with
            expr =
              (fun self expr ->
                let expr = super.expr self expr in
                match
                  List.find_map stage2_and_repls ~f:(fun (stage2, repl) ->
                      replace expr ~type_index ~stage2 ~repl)
                with
                | None -> expr
                | Some expr ->
                    changed_something := true;
                    expr)
          ; structure_item =
              update_migrate_test_payload
                ~match_attr:(__ =: "migrate_test.replace")
                super ~changed_something
          }
        in
        self.structure self structure)
