open Base
open Common
open Transform_common
open! Ocamlformat_ocaml_common
open Ocamlformat_parser_extended
module P = Parsetree
open! Fmast

(* Possible design:
   - __etc to match any number of tuple elements/list elements
   - it may be better for pexp_ident to match the value rather than syntactically,
     and have some mechanism for a syntactic match. Maybe [%exact foo]?
   - might be useful to have ways of stating some properties of the code like
     "cannot raise" or "duplicatable", such that you can say things like
     (try Some (Sys.getenv (__var & [%noraise])) with Not_found -> None) /// Sys.getenv_opt __var
*)

let map_ref_find_and_remove map key =
  match Map.find !map key with
  | None -> None
  | Some _ as opt ->
      map := Map.remove !map key;
      opt

let uid_of_var_binding ~index (binding : P.value_binding) =
  match binding.pvb_pat.ppat_desc with
  | Ppat_var _ -> (
      match Build.Type_index.pat index (Conv.location' binding.pvb_pat.ppat_loc) with
      | [] ->
          if !log then print_s [%sexp "no type"];
          None
      | T tpat :: _ -> (
          match tpat.pat_desc with Tpat_var (_, _, uid) -> Some uid | _ -> None))
  | _ -> None

let locate_def ~index structure uid =
  With_return.with_return (fun r ->
      let super = Ast_mapper.default_mapper in
      let self =
        { super with
          value_binding =
            (fun self binding ->
              match uid_of_var_binding ~index binding with
              | Some uid' when Shape.Uid.equal uid uid' -> (
                  match binding.pvb_body with
                  | Pfunction_body body -> r.return (Some body)
                  | Pfunction_cases _ -> assert false)
              | _ -> super.value_binding self binding)
        }
      in
      ignore (self.structure self structure);
      None)

let drop_defs ~type_index structure uids =
  let uids = Shape.Uid.Set.of_seq (Stdlib.List.to_seq uids) in
  if Shape.Uid.Set.is_empty uids
  then structure
  else
    let index = Option.value_exn (Lazy.force type_index) in
    let super = Ast_mapper.default_mapper in
    let self =
      { super with
        value_bindings =
          (fun self vbs ->
            let vbs = super.value_bindings self vbs in
            { vbs with
              pvbs_bindings =
                List.filter vbs.pvbs_bindings ~f:(fun binding ->
                    match uid_of_var_binding ~index binding with
                    | Some uid when Shape.Uid.Set.mem uid uids -> false
                    | _ -> true)
            })
      }
    in
    self.structure self structure

type value =
  | Expr of P.expression
  | Variant of string
  | Typ of P.core_type
  | Pat of P.pattern
  | Args of function_arg list
  | Fields of
      (Longident.t Location.loc * P.type_constraint option * expression option) list

type ctx =
  { type_index : Build.Type_index.t option Lazy.t
  ; whole_ast : Parsetree.structure
  }

type env =
  { bindings : value Map.M(String).t
  ; nodes_to_remove : Shape.Uid.t list
  }

type stage2 = Parsetree.expression -> env:env ref -> ctx:ctx -> bool

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
      match Map.add !env.bindings ~key:v_motif ~data:(make_data data) with
      | `Ok map ->
          env := { !env with bindings = map };
          true
      | `Duplicate -> false

let etc_field : Longident.t Location.loc * _ option * expression option -> _ = function
  | { txt = Lident v; _ }, None, Some { pexp_desc = Pexp_ident { txt = Lident v'; _ }; _ }
    when v =: v' && String.is_prefix v ~prefix:"__etc" ->
      Some v
  | { txt = Lident v; _ }, None, None when String.is_prefix v ~prefix:"__etc" -> Some v
  | _ -> None

let etc_arg : function_arg -> _ = function
  | Nolabel, { pexp_desc = Pexp_ident { txt = Lident v; _ }; _ } ->
      if String.is_prefix v ~prefix:"__etc" then Some v else None
  | _ -> None

let unsupported_motif loc =
  Location.raise_errorf ~loc:(Conv.location loc) "unsupported motif syntax"

let rec match_ ~need_type_index (motif : Uast.Parsetree.expression) : stage2 =
  match motif.pexp_desc with
  | Pexp_ident { txt = Lident v; _ } when String.is_prefix v ~prefix:"__" ->
      match_var v (fun e -> Expr e)
  | Pexp_constant c_m -> (
      fun expr ~env:_ ~ctx:_ ->
        match expr.pexp_desc with
        | Pexp_constant c -> (
            match (c_m, c.pconst_desc) with
            | Pconst_integer (s, c_opt), Pconst_integer (s2, c_opt2) ->
                [%equal: string * char option] (s, c_opt) (s2, c_opt2)
            | Pconst_char c, Pconst_char (c2, _) -> [%equal: char] c c2
            | Pconst_string (s, _, _), Pconst_string (s2, _, _) -> s =: s2
            | Pconst_float (s, _), Pconst_float (s2, _) -> s =: s2
            | _ -> false)
        | _ -> false)
  | Pexp_constraint (m1, typ) -> (
      need_type_index := true;
      let user_type = Uast.type_type typ in
      let stage1 = match_ ~need_type_index m1 in
      fun expr ~env ~ctx ->
        match Lazy.force ctx.type_index with
        | None ->
            if !log then print_s [%sexp "missing type index"];
            false
        | Some index -> (
            match Build.Type_index.expr index (Conv.location' expr.pexp_loc) with
            | [] ->
                if !log then print_s [%sexp "no type"];
                false
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
                && stage1 expr ~env ~ctx))
  | Pexp_ident id_motif -> (
      fun expr ~env:_ ~ctx:_ ->
        match expr.pexp_desc with
        | Pexp_ident id ->
            Fmast.Longident.compare id.txt (Conv.longident id_motif.txt) = 0
        | _ -> false)
  | Pexp_tuple motifs ->
      match_list (match_ ~need_type_index) motifs (function
        | ({ pexp_desc = Pexp_tuple es; _ } : P.expression) -> Some es
        | _ -> None)
  | Pexp_apply
      ( { pexp_desc = Pexp_ident { txt = Lident (("&" | "or") as op); _ }; _ }
      , [ (Nolabel, m1); (Nolabel, m2) ] ) -> (
      let s1 = match_ ~need_type_index m1 in
      let s2 = match_ ~need_type_index m2 in
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
      let sf = match_ ~need_type_index mf in
      let sargs = match_args ~need_type_index margs in
      fun expr ~env ~ctx ->
        match expr.pexp_desc with
        | Pexp_apply (f, args) -> sf f ~env ~ctx && sargs args ~env ~ctx
        | _ -> false)
  | Pexp_construct (id_motif, motif_opt) -> (
      let sopt = match_option (match_ ~need_type_index) motif_opt in
      fun expr ~env ~ctx ->
        match expr.pexp_desc with
        | Pexp_construct (id, eopt) ->
            Fmast.Longident.compare id.txt (Conv.longident id_motif.txt) = 0
            && sopt eopt ~env ~ctx
        | _ -> false)
  | Pexp_variant (label_motif, motif_opt) -> (
      let sopt = match_option (match_ ~need_type_index) motif_opt in
      fun expr ~env ~ctx ->
        match expr.pexp_desc with
        | Pexp_variant (label, eopt) ->
            label.txt.txt =: label_motif && sopt eopt ~env ~ctx
        | _ -> false)
  | Pexp_record (fields_motif, init_motif) -> (
      let s_init = match_option (match_ ~need_type_index) init_motif in
      let s_fields = match_fields ~need_type_index fields_motif in
      fun expr ~env ~ctx ->
        match expr.pexp_desc with
        | Pexp_record (fields, init) -> s_init init ~env ~ctx && s_fields fields ~env ~ctx
        | _ -> false)
  | Pexp_lazy m1 -> (
      let s1 = match_ ~need_type_index m1 in
      fun expr ~env ~ctx ->
        match expr.pexp_desc with Pexp_lazy e1 -> s1 e1 ~env ~ctx | _ -> false)
  | Pexp_sequence (m1, m2) -> (
      let s1 = match_ ~need_type_index m1 in
      let s2 = match_ ~need_type_index m2 in
      fun expr ~env ~ctx ->
        match expr.pexp_desc with
        | Pexp_sequence (e1, e2) -> s1 e1 ~env ~ctx && s2 e2 ~env ~ctx
        | _ -> false)
  | Pexp_function (params_motif, None, Pfunction_body body_motif) -> (
      let match_params =
        match_list (match_param ~need_type_index) params_motif (Some __)
      in
      let match_body = match_ ~need_type_index body_motif in
      fun expr ~env ~ctx ->
        match expr.pexp_desc with
        | Pexp_function (params, None, Pfunction_body body) ->
            match_params params ~env ~ctx && match_body body ~env ~ctx
        | _ -> false)
  | Pexp_extension (motif_name, motif_payload) -> (
      need_type_index := true;
      let s_payload = match_payload ~loc:motif.pexp_loc ~need_type_index motif_payload in
      match motif_name.txt with
      | "move_def" -> (
          fun expr ~env ~ctx ->
            match expr.pexp_desc with
            | Pexp_ident { txt = Lident _; _ } -> (
                match Lazy.force ctx.type_index with
                | None ->
                    if !log then print_s [%sexp "missing type index"];
                    false
                | Some index -> (
                    match Build.Type_index.expr index (Conv.location' expr.pexp_loc) with
                    | [] ->
                        if !log then print_s [%sexp "no type"];
                        false
                    | texpr :: _ -> (
                        match texpr.exp_desc with
                        | Texp_ident (_, _, vd) -> (
                            let uid = vd.val_uid in
                            match locate_def ~index ctx.whole_ast uid with
                            | None -> false
                            | Some def ->
                                env :=
                                  { !env with
                                    nodes_to_remove = uid :: !env.nodes_to_remove
                                  };
                                s_payload (P.PStr [ Ast_helper.Str.eval def ]) ~env ~ctx)
                        | _ -> assert false)))
            | _ -> false)
      | _ -> (
          fun expr ~env ~ctx ->
            match expr.pexp_desc with
            | Pexp_extension (name, payload) ->
                name.txt =: motif_name.txt && s_payload payload ~env ~ctx
            | _ -> false))
  | _ -> unsupported_motif motif.pexp_loc

and match_fields ~need_type_index
    (mfields : (Uast.Longident.t Uast.Location.loc * Uast.Parsetree.expression) list) =
  let var_other = ref None in
  let s_named = ref (Map.empty (module Longident)) in
  List.iter mfields ~f:(fun (id, m) ->
      match id.txt with
      | Lident id'
        when (match m.pexp_desc with
             | Pexp_ident { txt = Lident id''; _ } when id'' =: id' -> true
             | _ -> false)
             && String.is_prefix id' ~prefix:"__etc" ->
          var_other := Some id'
      | _ ->
          s_named :=
            Map.add_exn !s_named ~key:(Conv.longident id.txt)
              ~data:(Conv.longident id.txt, match_ ~need_type_index m));
  let var_other = !var_other in
  let s_named = !s_named in
  fun fields ~env ~ctx ->
    let s_named = ref s_named in
    let others = ref [] in
    List.for_all fields ~f:(fun (id, tyopt, value) ->
        match tyopt with
        | Some _ -> false
        | None -> (
            match map_ref_find_and_remove s_named id.txt with
            | Some (motif_id, stage2) ->
                Longident.compare motif_id id.txt = 0
                && stage2
                     (Option.value_exn value ~message:"should be normalized away")
                     ~env ~ctx
            | None ->
                if Option.is_some var_other
                then (
                  others := (id, tyopt, value) :: !others;
                  true)
                else false))
    && Map.is_empty !s_named
    &&
    match var_other with
    | None -> true
    | Some var_other -> (
        match Map.add !env.bindings ~key:var_other ~data:(Fields (List.rev !others)) with
        | `Ok map ->
            env := { !env with bindings = map };
            true
        | `Duplicate -> false)

and match_param ~need_type_index (p : Uast.Parsetree.function_param) =
  match p.pparam_desc with
  | Pparam_newtype name -> unsupported_motif name.loc
  | Pparam_val (_, Some e, _) -> unsupported_motif e.pexp_loc
  | Pparam_val (arg_label_m, None, pat_m) -> (
      let match_p = match_pat ~need_type_index pat_m in
      fun (p : P.expr_function_param) ~env ~ctx ->
        match p.pparam_desc with
        | Pparam_newtype _ -> false
        | Pparam_val (_, Some _, _) -> false
        | Pparam_val (arg_label, None, pat) ->
            Arg_label.equal (Conv.arg_label arg_label_m) arg_label
            && match_p pat ~env ~ctx)

and match_pat ~need_type_index (p : Uast.Parsetree.pattern) =
  match p.ppat_desc with
  | Ppat_var v_motif when String.is_prefix v_motif.txt ~prefix:"__" ->
      match_var v_motif.txt (fun p -> Pat p)
  | Ppat_variant (v, p2) -> (
      let s_label =
        if String.is_suffix v ~suffix:"__"
        then match_var v (fun t -> Variant t)
        else fun v' ~env:_ ~ctx:_ -> v =: v'
      in
      let s_payload = match_option (match_pat ~need_type_index) p2 in
      fun p ~env ~ctx ->
        match p.ppat_desc with
        | Ppat_variant (v, p2) -> s_label v.txt.txt ~env ~ctx && s_payload p2 ~env ~ctx
        | _ -> false)
  | _ -> unsupported_motif p.ppat_loc

and match_args ~need_type_index
    (margs : (Uast.Asttypes.arg_label * Uast.Parsetree.expression) list) =
  let var_other = ref None in
  let s_named = ref (Map.empty (module String)) in
  let s_anon = Queue.create () in
  List.iter margs ~f:(fun (arg_label, motif) ->
      match arg_label with
      | Nolabel -> (
          match motif.pexp_desc with
          | Pexp_ident { txt = Lident v; _ } when String.is_prefix v ~prefix:"__etc" ->
              var_other := Some v
          | _ -> Queue.enqueue s_anon (match_ ~need_type_index motif))
      | _ ->
          s_named :=
            Map.add_exn !s_named
              ~key:(Arg_label.to_string (Conv.arg_label arg_label))
              ~data:(Conv.arg_label arg_label, match_ ~need_type_index motif));
  let var_other = !var_other in
  let s_named = !s_named in
  let s_anon = Queue.to_list s_anon in
  fun args ~env ~ctx ->
    let s_anon = ref s_anon in
    let s_named = ref s_named in
    let others = ref [] in
    let match_others arg_label arg =
      if Option.is_some var_other
      then (
        others := (arg_label, arg) :: !others;
        true)
      else false
    in
    List.for_all args ~f:(fun (arg_label, arg) ->
        match arg_label with
        | Nolabel -> (
            match !s_anon with
            | stage2 :: rest ->
                s_anon := rest;
                stage2 arg ~env ~ctx
            | [] -> match_others arg_label arg)
        | _ -> (
            match
              map_ref_find_and_remove s_named (Fmast.Arg_label.to_string arg_label)
            with
            | Some (motif_arg_label, stage2) ->
                (* label comparison probably not ideal, for optional arguments? *)
                Arg_label.equal motif_arg_label arg_label && stage2 arg ~env ~ctx
            | None -> match_others arg_label arg))
    && Map.is_empty !s_named
    &&
    match var_other with
    | None -> true
    | Some var_other -> (
        match Map.add !env.bindings ~key:var_other ~data:(Args (List.rev !others)) with
        | `Ok map ->
            env := { !env with bindings = map };
            true
        | `Duplicate -> false)

and match_payload ~need_type_index ~loc (motif : Uast.Parsetree.payload) =
  match motif with
  | PStr m -> (
      let sm = match_structure ~need_type_index ~loc m in
      fun s ~env ~ctx -> match s with PStr si -> sm si ~env ~ctx | _ -> false)
  | _ -> unsupported_motif loc

and match_structure ~loc ~need_type_index (stritems : Uast.Parsetree.structure_item list)
    =
  match_list (match_structure_item ~loc ~need_type_index) stritems Option.some

and match_structure_item ~loc ~need_type_index (motif : Uast.Parsetree.structure_item) =
  match motif.pstr_desc with
  | Pstr_eval (motif1, _) -> (
      let s1 = match_ ~need_type_index motif1 in
      fun (s : P.structure_item) ~env ~ctx ->
        match s.pstr_desc with Pstr_eval (e, _) -> s1 e ~env ~ctx | _ -> false)
  | _ -> unsupported_motif loc

let subst meth v ~env =
  let super = Ast_mapper.default_mapper in
  let self =
    { super with
      typ =
        (fun self ty ->
          let ty = super.typ self ty in
          match ty.ptyp_desc with
          | Ptyp_constr ({ txt = Lident v; _ }, []) when Map.mem env v -> (
              match Map.find_exn env v with
              | Typ ty -> ty
              | Fields _ | Expr _ | Variant _ | Args _ | Pat _ ->
                  Location.raise_errorf ~loc:ty.ptyp_loc
                    "motif %s can't be inserted in a type" v)
          | _ -> ty)
    ; pat =
        (fun self pat ->
          let pat = super.pat self pat in
          match pat.ppat_desc with
          | Ppat_var { txt = var; _ } when Map.mem env var -> (
              match Map.find_exn env var with
              | Pat p -> p
              | Fields _ | Expr _ | Variant _ | Args _ | Typ _ ->
                  Location.raise_errorf ~loc:pat.ppat_loc
                    "motif %s can't be inserted in a pattern" var)
          | Ppat_variant (var, p2) when Map.mem env var.txt.txt -> (
              match Map.find_exn env var.txt.txt with
              | Variant var' ->
                  { pat with
                    ppat_desc =
                      Ppat_variant ({ var with txt = { var.txt with txt = var' } }, p2)
                  }
              | Fields _ | Expr _ | Pat _ | Args _ | Typ _ ->
                  Location.raise_errorf ~loc:pat.ppat_loc
                    "motif %s can't be inserted in a pattern" var.txt.txt)
          | _ -> pat)
    ; expr =
        with_log (fun self expr ->
            let expr =
              match expr.pexp_desc with
              | Pexp_record (fields, init) ->
                  let fields' =
                    List.concat_map fields ~f:(fun ((id, typopt, value) as field) ->
                        match etc_field field with
                        | Some v -> (
                            match Map.find_exn env v with
                            | Expr _ | Args _ | Pat _ | Typ _ | Variant _ ->
                                Location.raise_errorf ~loc:id.loc "hm, what"
                            | Fields fs -> fs)
                        | None ->
                            [ ( id
                              , typopt
                                (* should map over the type constraint here, or perhaps
                                 normalize this construction away *)
                              , Option.map ~f:(self.expr self) value )
                            ])
                  in
                  let init' = Option.map ~f:(self.expr self) init in
                  { expr with pexp_desc = Pexp_record (fields', init') }
              | Pexp_apply (f, args) when Option.is_some (List.find_map args ~f:etc_arg)
                ->
                  let f' = self.expr self f in
                  let args' =
                    List.concat_map args ~f:(fun (arg_label, arg) ->
                        match etc_arg (arg_label, arg) with
                        | Some v -> (
                            match Map.find_exn env v with
                            | Expr _ | Fields _ | Pat _ | Typ _ | Variant _ ->
                                Location.raise_errorf ~loc:arg.pexp_loc "hm, what"
                            | Args args -> args)
                        | None -> [ (arg_label, self.expr self arg) ])
                  in
                  { expr with pexp_desc = Pexp_apply (f', args') }
              | _ -> super.expr self expr
            in
            let expr = super.expr self expr in
            match expr.pexp_desc with
            | Pexp_record (fields, init)
              when not (Attr.exists expr.pexp_attributes (Attr.reorder `Internal)) ->
                let fields' =
                  fields
                  |> List.map ~f:(fun (id, typopt, eopt) ->
                         ((id, typopt), Option.value_exn eopt))
                  |> Transform_migration.commute_list (fun _ _ -> true)
                  |> List.map ~f:(fun ((id, typopt), e) -> (id, typopt, Some e))
                in
                { expr with pexp_desc = Pexp_record (fields', init) }
            | Pexp_apply (f, args)
              when not (Attr.exists expr.pexp_attributes (Attr.reorder `Internal)) ->
                let args' = Transform_migration.commute_args args in
                { expr with pexp_desc = Pexp_apply (f, args') }
            | Pexp_ident { txt = Lident var; _ } when Map.mem env var -> (
                match Map.find_exn env var with
                | Expr e -> e
                | Fields _ | Pat _ | Typ _ | Args _ | Variant _ ->
                    Location.raise_errorf ~loc:expr.pexp_loc
                      "motif %s cannot be inserted into an expression" var)
            | _ -> expr)
    }
  in
  (meth self) self v

let replace (meth, preserve_loc) expr ~whole_ast ~type_index ~stage2 ~repl =
  let env = ref { bindings = Map.empty (module String); nodes_to_remove = [] } in
  if stage2 expr ~env ~ctx:{ type_index; whole_ast }
  then
    let repl = preserve_loc repl in
    Some (subst meth repl ~env:!env.bindings, !env.nodes_to_remove)
  else None

let rec match_type ~need_type_index (t : Uast.Parsetree.core_type) =
  match t.ptyp_desc with
  | Ptyp_constr ({ txt = Lident v; _ }, []) when String.is_prefix v ~prefix:"__" ->
      match_var v (fun t -> Typ t)
  | Ptyp_variant
      ([ { prf_desc = Rtag (label, has_no_payload, types); _ } ], closed_flag, None)
    when Bool.( = ) has_no_payload (List.is_empty types) -> (
      let s_label =
        if String.is_suffix label.txt ~suffix:"__"
        then match_var label.txt (fun t -> Variant t)
        else fun label' ~env:_ ~ctx:_ -> label.txt =: label'
      in
      let s_typ =
        match types with
        | [] ->
            fun (has_no_payload, types) ~ctx:_ ~env:_ ->
              has_no_payload && List.is_empty types
        | m_typ :: _ -> (
            let s_typ = match_type ~need_type_index m_typ in
            fun (has_no_payload, types) ~ctx ~env ->
              (not has_no_payload)
              && match types with [ typ ] -> s_typ typ ~ctx ~env | _ -> false)
      in
      fun typ ~env ~ctx ->
        match typ with
        | { ptyp_desc =
              Ptyp_variant
                ( [ { prf_desc = Rtag (label, has_no_payload, types); _ } ]
                , closed_flag'
                , None )
          ; _
          } ->
            s_label label.txt.txt ~env ~ctx
            && (match (closed_flag, closed_flag') with
               | Open, Open | Closed, Closed -> true
               | _ -> false)
            && s_typ (has_no_payload, types) ~env ~ctx
        | _ -> false)
  | _ -> unsupported_motif t.ptyp_loc

let match_value_binding ~need_type_index
    ({ pvb_pat; pvb_expr; pvb_constraint; pvb_attributes = _; pvb_loc } :
      Uast.Parsetree.value_binding) =
  match pvb_constraint with
  | Some _ -> unsupported_motif pvb_loc
  | None -> (
      let s_pat = match_pat ~need_type_index pvb_pat in
      let s_expr = match_ ~need_type_index pvb_expr in
      fun (vb : P.value_binding) ~env ~ctx ->
        Option.is_none vb.pvb_constraint
        && s_pat vb.pvb_pat ~env ~ctx
        &&
        match vb.pvb_body with
        | Pfunction_cases _ -> assert false
        | Pfunction_body body -> s_expr body ~env ~ctx)

let split_bop str = (String.prefix str 3, String.drop_prefix str 3)

let match_binding_op ~need_type_index
    ({ pbop_op; pbop_pat; pbop_exp; pbop_loc = _ } : Uast.Parsetree.binding_op) =
  (* turn let+/and+ into + *)
  let m_op = split_bop pbop_op.txt in
  let s_pat = match_pat ~need_type_index pbop_pat in
  let s_expr = match_ ~need_type_index pbop_exp in
  fun (op : P.binding_op) ~env ~ctx ->
    snd m_op =: snd (split_bop op.pbop_op.txt)
    && s_pat op.pbop_pat ~env ~ctx
    && s_expr op.pbop_exp ~env ~ctx

let compile_motif ~need_type_index (motif : Uast.Parsetree.expression) =
  match motif.pexp_desc with
  | Pexp_extension ({ txt = "binding"; _ }, payload) -> (
      match payload with
      | PStr
          [ { pstr_desc =
                Pstr_eval ({ pexp_desc = Pexp_let (Nonrecursive, [ vb ], _); _ }, _)
            ; _
            }
          ] ->
          `Binding (match_value_binding ~need_type_index vb)
      | PStr
          [ { pstr_desc =
                Pstr_eval
                  ( { pexp_desc = Pexp_letop { let_ = binding_op; ands = []; body = _ }
                    ; _
                    }
                  , _ )
            ; _
            }
          ] ->
          `Binding_op (match_binding_op ~need_type_index binding_op)
      | _ -> unsupported_motif motif.pexp_loc)
  | Pexp_extension ({ txt = "type"; _ }, payload) -> (
      match payload with
      | PTyp typ -> `Type (match_type ~need_type_index typ)
      | _ -> unsupported_motif motif.pexp_loc)
  | _ -> `Expr (match_ ~need_type_index motif)

let parse_template ~fmconf stage2 repl =
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
  match stage2 with
  | `Expr stage2 -> `Expr (stage2, repl)
  | `Type stage2 -> (
      match repl.pexp_desc with
      | Pexp_extension ({ txt = "type"; _ }, PTyp typ) -> `Type (stage2, typ)
      | _ -> unsupported_motif (Conv.location' repl.pexp_loc))
  | (`Binding _ | `Binding_op _) as stage2 -> (
      match (stage2, repl.pexp_desc) with
      | `Binding stage2, Pexp_extension ({ txt = "binding"; _ }, payload) -> (
          match payload with
          | PStr
              [ { pstr_desc =
                    Pstr_eval
                      ( { pexp_desc =
                            Pexp_let
                              ({ pvbs_rec = Nonrecursive; pvbs_bindings = [ vb ] }, _, _)
                        ; _
                        }
                      , _ )
                ; _
                }
              ] ->
              `Binding (stage2, vb)
          | _ -> unsupported_motif (Conv.location' repl.pexp_loc))
      | `Binding_op stage2, Pexp_extension ({ txt = "binding"; _ }, payload) -> (
          match payload with
          | PStr
              [ { pstr_desc =
                    Pstr_eval
                      ( { pexp_desc = Pexp_letop { let_ = binding_op; ands = []; _ }; _ }
                      , _ )
                ; _
                }
              ] ->
              `Binding_op (stage2, binding_op)
          | _ -> unsupported_motif (Conv.location' repl.pexp_loc))
      | _ -> unsupported_motif (Conv.location' repl.pexp_loc))

let run motif_and_repls () =
  let may_need_type_index_ref = ref false in
  let stage2_and_repls =
    List.map motif_and_repls ~f:(fun (motif, repl) ->
        let motif =
          Uast.Parse.expression (lexing_from_string motif ~file_path:"command line param")
        in
        let repl = ref (`Unforced repl) in
        (compile_motif ~need_type_index:may_need_type_index_ref motif, repl))
  in
  fun ~fmconf ~type_index ~source_path ~input_name_matching_compilation_command ->
    let stage2_and_repls =
      List.map stage2_and_repls ~f:(fun (stage2, repl) ->
          parse_template ~fmconf stage2 repl)
    in
    let input_name_matching_compilation_command =
      if !may_need_type_index_ref
      then force input_name_matching_compilation_command
      else None
    in
    let all_nodes_to_remove = Queue.create () in
    process_file ~fmconf ~source_path ~input_name_matching_compilation_command
      (fun changed_something structure ->
        let super = Ast_mapper.default_mapper in
        let self =
          { super with
            typ =
              (fun self ty ->
                let ty = super.typ self ty in
                match
                  List.find_map stage2_and_repls ~f:(function
                    | `Expr _ | `Binding_op _ | `Binding _ -> None
                    | `Type (stage2, repl) ->
                        replace
                          ( __.typ
                          , preserve_loc_to_preserve_comment_pos __.typ ~from:ty.ptyp_loc
                          )
                          ty ~whole_ast:structure ~type_index ~stage2 ~repl)
                with
                | None -> ty
                | Some (ty, nodes_to_remove) ->
                    Queue.enqueue_all all_nodes_to_remove nodes_to_remove;
                    changed_something := true;
                    ty)
          ; value_binding =
              (fun self vb ->
                let vb = super.value_binding self vb in
                match
                  List.find_map stage2_and_repls ~f:(function
                    | `Expr _ | `Binding_op _ | `Type _ -> None
                    | `Binding (stage2, repl) ->
                        replace
                          ( __.value_binding
                          , preserve_loc_to_preserve_comment_pos __.value_binding
                              ~from:vb.pvb_loc )
                          vb ~whole_ast:structure ~type_index ~stage2 ~repl)
                with
                | None -> vb
                | Some (vb, nodes_to_remove) ->
                    Queue.enqueue_all all_nodes_to_remove nodes_to_remove;
                    changed_something := true;
                    vb)
          ; binding_op =
              (fun self bop ->
                let bop = super.binding_op self bop in
                match
                  List.find_map stage2_and_repls ~f:(function
                    | `Expr _ | `Binding _ | `Type _ -> None
                    | `Binding_op (stage2, repl) ->
                        let repl =
                          { repl with
                            pbop_op =
                              { txt =
                                  fst (split_bop bop.pbop_op.txt)
                                  ^ snd (split_bop repl.pbop_op.txt)
                              ; loc = repl.pbop_op.loc
                              }
                          }
                        in
                        replace
                          ( __.binding_op
                          , preserve_loc_to_preserve_comment_pos __.binding_op
                              ~from:bop.pbop_loc )
                          bop ~whole_ast:structure ~type_index ~stage2 ~repl)
                with
                | None -> bop
                | Some (bop, nodes_to_remove) ->
                    Queue.enqueue_all all_nodes_to_remove nodes_to_remove;
                    changed_something := true;
                    bop)
          ; expr =
              (fun self expr ->
                let expr = super.expr self expr in
                match
                  List.find_map stage2_and_repls ~f:(function
                    | `Binding _ | `Binding_op _ | `Type _ -> None
                    | `Expr (stage2, repl) ->
                        replace
                          (__.expr, preserve_loc_to_preserve_comment_pos_expr ~from:expr)
                          expr ~whole_ast:structure ~type_index ~stage2 ~repl)
                with
                | None -> expr
                | Some (expr, nodes_to_remove) ->
                    Queue.enqueue_all all_nodes_to_remove nodes_to_remove;
                    changed_something := true;
                    expr)
          ; structure_item =
              update_migrate_test_payload
                ~match_attr:(__ =: "migrate_test.replace")
                super ~changed_something
          }
        in
        let structure = self.structure self structure in
        drop_defs ~type_index structure (Queue.to_list all_nodes_to_remove))
