open Base
open Common
open Transform_common

let conv_arg_label : Asttypes.arg_label -> Ocamlformat_parser_extended.Asttypes.arg_label
    = function
  | Nolabel -> Nolabel
  | Labelled s -> Labelled { txt = s; loc = Fmast.Location.none }
  | Optional s -> Optional { txt = s; loc = Fmast.Location.none }

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
   - __etc to match any number of arguments/tuple elements/list elements
     Or maybe it could be __foo*, with a simple rewriting of such tokens.
   - When rewriting an application, we should reorder arguments, same as in the migration
     code
   - it may be better for pexp_ident to match the value rather than the syntactically,
     and have some mechanism for a syntactic match. Maybe [%exact foo]?
   - might be useful to have ways of stating some properties of the code like
     "cannot raise" or "duplicatable". Perhaps we could support general conjunction
     and disjunction with (e1 & e2) and (e1 or e2), such that you can say things like
     (try Some (Sys.getenv (__var & [%noraise])) with Not_found -> None) /// Sys.getenv_opt __var

   The matching should probably be staged (match on the pattern first). At least it
   would allow us to give errors early, and compile types once and for all.
*)

let arg_label_equal (l1 : Asttypes.arg_label) (l2 : Asttypes.arg_label) =
  match (l1, l2) with
  | Nolabel, Nolabel -> true
  | Labelled s1, Labelled s2 -> s1.txt =: s2.txt
  | Optional s1, Optional s2 -> s1.txt =: s2.txt
  | (Nolabel | Labelled _ | Optional _), _ -> false

let match_list l1 l2 match_a =
  match List.for_all2 l1 l2 ~f:match_a with Unequal_lengths -> false | Ok b -> b

let string_from_arg_label : Asttypes.arg_label -> _ = function
  | Asttypes.Nolabel -> ""
  | Labelled s | Optional s -> s.txt

let alabel (arg_label, _) = string_from_arg_label arg_label
let alabel' (arg_label, _) = string_from_arg_label (conv_arg_label arg_label)

let plabel (p : P.expr_function_param) =
  match p.pparam_desc with
  | Pparam_newtype _ -> ""
  | Pparam_val (label, _, _) -> string_from_arg_label label

let plabel' (p : Uast.Parsetree.function_param) =
  match p.pparam_desc with
  | Pparam_newtype _ -> ""
  | Pparam_val (label, _, _) -> string_from_arg_label (conv_arg_label label)

let rec match_args args args_pattern ~env ~ctx =
  let args =
    List.stable_sort args ~compare:(fun a1 a2 -> String.compare (alabel a1) (alabel a2))
  in
  let args_pattern =
    List.stable_sort args_pattern ~compare:(fun arg1 arg2 ->
        String.compare (alabel' arg1) (alabel' arg2))
  in
  match_list args args_pattern (fun (a_label, a) (p_label, p) ->
      arg_label_equal a_label (conv_arg_label p_label) && match_ a p ~env ~ctx)

and match_params (params : P.expr_function_param list)
    (params_pattern : Uast.Parsetree.function_param list) ~env ~ctx =
  let params =
    List.stable_sort params ~compare:(fun a1 a2 -> String.compare (plabel a1) (plabel a2))
  in
  let params_pattern =
    List.stable_sort params_pattern ~compare:(fun arg1 arg2 ->
        String.compare (plabel' arg1) (plabel' arg2))
  in
  match_list params params_pattern (fun param param_pattern ->
      match (param.pparam_desc, param_pattern.pparam_desc) with
      | Pparam_newtype _, _ | _, Pparam_newtype _ -> false
      | Pparam_val (label, None, pattern), Pparam_val (label', None, pattern_pattern) ->
          arg_label_equal label (conv_arg_label label')
          && match_pattern pattern pattern_pattern ~env ~ctx
      | _ -> false)

and match_pattern (p : P.pattern) (pp : Uast.Parsetree.pattern) ~env ~ctx:_ =
  match (p.ppat_desc, pp.ppat_desc) with
  | _, Ppat_var { txt = v; _ } when String.is_prefix v ~prefix:"__" -> (
      v =: "__" (* has a dedicated branch to support multiple __ patterns *)
      ||
      match Map.add !env ~key:v ~data:(`Pat p) with
      | `Ok map ->
          env := map;
          true
      | `Duplicate -> false)
  | _ -> false

and match_ (expr : P.expression) (pattern : Uast.Parsetree.expression) ~env ~ctx =
  match (expr.pexp_desc, pattern.pexp_desc) with
  | _, Pexp_ident { txt = Lident v; _ } when String.is_prefix v ~prefix:"__" -> (
      match Map.add !env ~key:v ~data:(`Expr expr) with
      | `Ok map ->
          env := map;
          true
      | `Duplicate -> false)
  | _, Pexp_constraint (p, typ) -> (
      match Lazy.force ctx with
      | None ->
          if !log then print_s [%sexp "missing type index"];
          false
      | Some index -> (
          match Build.Type_index.expr index (conv_location' expr.pexp_loc) with
          | texpr :: _ ->
              (let user_type =
                 Uast.type_type typ (* we should not do this in a loop! *)
               in
               let does_match =
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
              && match_ expr p ~env ~ctx
          | [] ->
              if !log then print_s [%sexp "no type"];
              false))
  | Pexp_ident id, Pexp_ident id2 ->
      Fmast.Longident.compare id.txt (conv_longident id2.txt) = 0
  | Pexp_tuple es, Pexp_tuple ps -> match_list es ps (match_ ~env ~ctx)
  | ( _
    , Pexp_apply
        ( { pexp_desc = Pexp_ident { txt = Lident (("&" | "or") as op); _ }; _ }
        , [ (Nolabel, p1); (Nolabel, p2) ] ) ) -> (
      match op with
      | "&" -> match_ expr p1 ~env ~ctx && match_ expr p2 ~env ~ctx
      | "or" ->
          (* This kind of eager matching may not be enough? *)
          let env_before = !env in
          match_ expr p1 ~env ~ctx
          ||
          (env := env_before;
           match_ expr p2 ~env ~ctx)
      | _ -> assert false)
  | Pexp_apply (f, args), Pexp_apply (f_pattern, args_pattern) ->
      match_ f f_pattern ~env ~ctx && match_args args args_pattern ~env ~ctx
  | Pexp_construct (id, eopt), Pexp_construct (id2, popt) ->
      Fmast.Longident.compare id.txt (conv_longident id2.txt) = 0
      && match_option eopt popt ~env ~ctx
  | Pexp_function (params, None, Pfunction_body body), Pexp_function (pparams, None, pbody)
    -> (
      match_params params pparams ~env ~ctx
      &&
      match pbody with
      | Pfunction_cases _ -> false
      | Pfunction_body body_p -> match_ body body_p ~env ~ctx)
  | Pexp_extension (name, payload), Pexp_extension (name_p, payload_p) ->
      name.txt =: name_p.txt && match_payload payload payload_p ~env ~ctx
  | _ -> false

and match_option o1 o2 ~env ~ctx =
  match (o1, o2) with
  | None, None -> true
  | Some e, Some p -> match_ e p ~env ~ctx
  | _ -> false

and match_payload payload payload_p ~env ~ctx =
  match (payload, payload_p) with
  | PStr s, PStr p -> match_structure s p ~env ~ctx
  | _ -> false

and match_structure xs ps ~env ~ctx = match_list xs ps (match_structure_item ~env ~ctx)

and match_structure_item (s : P.structure_item) p ~env ~ctx =
  match (s.pstr_desc, p.pstr_desc) with
  | Pstr_eval (e, _), Pstr_eval (p, _) -> match_ e p ~env ~ctx
  | _ -> false

let subst expr ~env =
  let super = Ast_mapper.default_mapper in
  let self =
    { super with
      pat =
        (fun self pat ->
          let pat = super.pat self pat in
          match pat.ppat_desc with
          | Ppat_var { txt = var; _ } when Map.mem env var -> (
              match Map.find_exn env var with
              | `Pat p -> p
              | `Expr _ ->
                  failwith
                    (Printf.sprintf "can't insert expression %s into a pattern" var))
          | _ -> pat)
    ; expr =
        with_log (fun self expr ->
            let expr = super.expr self expr in
            match expr.pexp_desc with
            | Pexp_ident { txt = Lident var; _ } when Map.mem env var -> (
                match Map.find_exn env var with
                | `Expr e -> e
                | `Pat _ ->
                    failwith
                      (Printf.sprintf "can't insert pattern %s into an expression" var))
            | _ -> expr)
    }
  in
  self.expr self expr

let replace expr ~type_index ~pattern ~repl =
  let env = ref (Map.empty (module String)) in
  if match_ expr pattern ~env ~ctx:type_index
  then
    let repl = preserve_loc_to_preserve_comment_pos ~from:expr repl in
    Some (subst repl ~env:!env)
  else None

let run pattern_and_repls () =
  let may_need_type_index_ref = ref false in
  let pattern_and_repls =
    List.map pattern_and_repls ~f:(fun (pattern, repl) ->
        let pattern =
          Uast.Parse.expression
            (lexing_from_string pattern ~file_path:"command line param")
        in
        if may_need_type_index pattern then may_need_type_index_ref := true;
        let repl = ref (`Unforced repl) in
        (pattern, repl))
  in
  fun ~fmconf ~type_index ~source_path ~input_name_matching_compilation_command ->
    let pattern_and_repls =
      List.map pattern_and_repls ~f:(fun (pattern, repl) ->
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
                in
                repl := `Forced v;
                v
          in
          (pattern, repl))
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
                  List.find_map pattern_and_repls ~f:(fun (pattern, repl) ->
                      replace expr ~type_index ~pattern ~repl)
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
