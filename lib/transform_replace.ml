open Base
open Common
open Transform_common
open! Ocamlformat_ocaml_common
open Ocamlformat_parser_extended
module P = Parsetree
open! Fmast
module Rep = Transform_replace_repetition

(* Thoughts about the design:
   - not clear if identifiers should be matched syntactically or by val_uid.
     Currently they are matched syntactically by default, or by uid when
     written as [%id Foo.x]
   - Rewriting would be more powerful if the "///" could be nested, such
     that we could rewrite under context:
     f (motif1 /// repl1) (motif2 /// repl2)
     That could be useful for changing a definition and a caller at once:
     [%def fun ?f __etc -> __body /// fun __etc -> __body] ~f:[%absent] __etc
   - maybe it should be possible to express more indirect rewriting rules.
     Say (fun a -> try Some (Sys.getenv a) with Not_found -> None) /// Sys.getenv_opt
     The advantage, compared to an hypothetical explicit version like
     (try Some (Sys.getenv (__var & [%noraise])) with Not_found -> None)
     is that the burden on ocamlmig to figure out under what constraints is the
     rewrite rule correct, and the user doesn't need to learn the syntax to express
     such conditions.
   - Maybe we should provide a way to upgrade an input to this module
     into an ocaml program that calls ocamlmig as a library, to help people upgrade
     to a real language when a rewriting gets complicated.
*)

let map_ref_find_and_remove map key =
  match Map.find !map key with
  | None -> None
  | Some _ as opt ->
      map := Map.remove !map key;
      opt

let uid_of_var_binding ~index (binding : P.value_binding) =
  match binding.pvb_pat with
  | { ppat_desc = Ppat_var _; _ } -> (
      match Build.Type_index.pat index (Conv.location' binding.pvb_pat.ppat_loc) with
      | [] -> None
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
      if !log || debug.all
      then print_s [%sexp "couldn't locate def for", (uid : Uast.Shape.Uid.t)];
      None)

let drop_defs ~type_index file_type structure uids =
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
    File_type.map file_type self structure

type value =
  | Expr of expression
  | Variant of string
  | Typ of core_type
  | Pat of pattern
  | Args of (function_arg list[@sexp.opaque])
  | Fields of
      (Longident.t Location.loc
      * (P.type_constraint option[@sexp.opaque])
      * expression option)
      list
[@@deriving sexp_of]

type ctx =
  { type_index : Build.Type_index.t option Lazy.t
  ; whole_ast : Parsetree.structure option
  ; new_base_env : (Build.Type_index.t * Env.t) option Lazy.t
  }

type ctx1 = { need_type_index : bool ref }

type env =
  { bindings : value Map.M(String).t
  ; xbindings : value Map.M(String).t
  ; xrest : value option
  ; nodes_to_remove : Shape.Uid.t list
  }

let env_pat ~loc env var =
  match Map.find_exn env var with
  | Pat v -> v
  | Expr _ | Fields _ | Typ _ | Args _ | Variant _ ->
      Location.raise_errorf ~loc "motif %s cannot be inserted into a pattern" var

let env_exp ~loc env var =
  match Map.find_exn env var with
  | Expr v -> v
  | Pat _ | Fields _ | Typ _ | Args _ | Variant _ ->
      Location.raise_errorf ~loc "motif %s cannot be inserted into an expression" var

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

let same_value (v1 : value) (v2 : value) =
  match (v1, v2) with
  | ( Expr { pexp_desc = Pexp_constant { pconst_desc = Pconst_integer (n, None); _ }; _ }
    , Expr { pexp_desc = Pexp_constant { pconst_desc = Pconst_integer (n', None); _ }; _ }
    ) ->
      (* for __count *)
      n =: n'
  | _ -> false

let match_var_snd_stage ?(x = false) ~env v_motif data =
  match Map.add (if x then !env.xbindings else !env.bindings) ~key:v_motif ~data with
  | `Ok map ->
      env := if x then { !env with xbindings = map } else { !env with bindings = map };
      true
  | `Duplicate -> (
      match Map.find (if x then !env.xbindings else !env.bindings) v_motif with
      | Some data' when same_value data data' -> true
      | _ -> false)

let match_var ?x v_motif make_data =
  if v_motif =: "__" (* has a dedicated branch to support multiple __ patterns *)
  then fun _ ~env:_ ~ctx:_ -> true
  else fun data ~env ~ctx:_ -> match_var_snd_stage ?x ~env v_motif (make_data data)

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

let unsupported_motif loc = Location.raise_errorf ~loc "unsupported motif syntax"
let match_count ~env n = match_var_snd_stage ~env "__count" (Expr (Ast_helper.Exp.int n))

let rec match_ ~ctx1 (motif : expression) : stage2 =
  match motif.pexp_desc with
  | Pexp_ident { txt = Lident v; _ } when String.is_prefix v ~prefix:"__" ->
      match_var v (fun e -> Expr e)
  | Pexp_constant c_m -> (
      fun expr ~env:_ ~ctx:_ ->
        match expr.pexp_desc with
        | Pexp_constant c -> (
            match (c_m.pconst_desc, c.pconst_desc) with
            | Pconst_integer (s, c_opt), Pconst_integer (s2, c_opt2) ->
                [%equal: string * char option] (s, c_opt) (s2, c_opt2)
            | Pconst_char (c, _), Pconst_char (c2, _) -> [%equal: char] c c2
            | Pconst_string (s, _, _), Pconst_string (s2, _, _) -> s =: s2
            | Pconst_float (s, _), Pconst_float (s2, _) -> s =: s2
            | _ -> false)
        | _ -> false)
  | Pexp_constraint (m1, typ) -> (
      ctx1.need_type_index := true;
      let user_type = Uast.type_type (utype_of_fmtype typ) in
      let stage1 = match_ ~ctx1 m1 in
      fun expr ~env ~ctx ->
        match Lazy.force ctx.type_index with
        | None ->
            if !log || debug.all then print_s [%sexp "missing type index"];
            false
        | Some index -> (
            match Build.Type_index.exp index (Conv.location' expr.pexp_loc) with
            | [] ->
                if !log || debug.all then print_s [%sexp "pexp_constraint", "no type"];
                false
            | texpr :: _ ->
                (let does_match =
                   Uast.match_typ ~env:texpr.exp_env texpr.exp_type ~user_type
                 in
                 if !log || debug.all
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
  | Pexp_extension
      ( { txt = "id"; loc = _ }
      , PStr [ { pstr_desc = Pstr_eval (({ pexp_desc = Pexp_ident _; _ } as e), _); _ } ]
      ) -> (
      let uid =
        (* How to name identifiers not visible from outside a library though? This
           doesn't allow naming Ocamlmig.Building.Listing.create for instance, since
           only the main function is exposed out of the ocamlmig library. Well, actually
           you can use Ocamlmig__.Build.Listing.create... Not the most stable though. *)
        match Typecore.type_expression (Compmisc.initial_env ()) (uexpr_of_fmexpr e) with
        | { exp_desc = Texp_ident (_, _, vd); _ } -> vd.val_uid
        | _ -> assert false
      in
      ctx1.need_type_index := true;
      fun expr ~env:_ ~ctx ->
        match expr.pexp_desc with
        | Pexp_ident _ -> (
            match Lazy.force ctx.type_index with
            | None ->
                if !log || debug.all then print_s [%sexp "missing type index"];
                false
            | Some index -> (
                match Build.Type_index.exp index (Conv.location' expr.pexp_loc) with
                | [] ->
                    if !log || debug.all then print_s [%sexp "id motif", "no type"];
                    false
                | texpr :: _ -> (
                    match texpr.exp_desc with
                    | Texp_ident (_, _, vd) -> Shape.Uid.equal uid vd.val_uid
                    | _ -> false)))
        | _ -> false)
  | Pexp_ident id_motif -> (
      fun expr ~env:_ ~ctx:_ ->
        match expr.pexp_desc with
        | Pexp_ident id -> Fmast.Longident.compare id.txt id_motif.txt = 0
        | _ -> false)
  | Pexp_tuple motifs ->
      match_list (match_ ~ctx1) motifs (function
        | ({ pexp_desc = Pexp_tuple es; _ } : P.expression) -> Some es
        | _ -> None)
  | Pexp_apply
      ( { pexp_desc = Pexp_ident { txt = Lident (("&" | "or") as op); _ }; _ }
      , [ (Nolabel, m1); (Nolabel, m2) ] ) -> (
      let s1 = match_ ~ctx1 m1 in
      let s2 = match_ ~ctx1 m2 in
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
      let sf = match_ ~ctx1 mf in
      let sargs = match_args ~ctx1 margs in
      fun expr ~env ~ctx ->
        match expr.pexp_desc with
        | Pexp_apply (f, args) -> sf f ~env ~ctx && sargs args ~env ~ctx
        | _ -> false)
  | Pexp_construct (id_motif, motif_opt) -> (
      let sopt = match_option (match_ ~ctx1) motif_opt in
      fun expr ~env ~ctx ->
        match expr.pexp_desc with
        | Pexp_construct (id, eopt) ->
            Fmast.Longident.compare id.txt id_motif.txt = 0 && sopt eopt ~env ~ctx
        | _ -> false)
  | Pexp_variant (label_motif, motif_opt) -> (
      let sopt = match_option (match_ ~ctx1) motif_opt in
      fun expr ~env ~ctx ->
        match expr.pexp_desc with
        | Pexp_variant (label, eopt) ->
            label.txt.txt =: label_motif.txt.txt && sopt eopt ~env ~ctx
        | _ -> false)
  | Pexp_record (fields_motif, init_motif) -> (
      let s_init = match_option (match_ ~ctx1) init_motif in
      let s_fields = match_fields ~ctx1 fields_motif in
      fun expr ~env ~ctx ->
        match expr.pexp_desc with
        | Pexp_record (fields, init) -> s_init init ~env ~ctx && s_fields fields ~env ~ctx
        | _ -> false)
  | Pexp_lazy m1 -> (
      let s1 = match_ ~ctx1 m1 in
      fun expr ~env ~ctx ->
        match expr.pexp_desc with Pexp_lazy e1 -> s1 e1 ~env ~ctx | _ -> false)
  | Pexp_sequence (m1, m2) -> (
      let s1 = match_ ~ctx1 m1 in
      let s2 = match_ ~ctx1 m2 in
      fun expr ~env ~ctx ->
        match expr.pexp_desc with
        | Pexp_sequence (e1, e2) -> s1 e1 ~env ~ctx && s2 e2 ~env ~ctx
        | _ -> false)
  | Pexp_function (params_motif, None, Pfunction_body body_motif) -> (
      let match_params = match_list (match_param ~ctx1) params_motif (Some __) in
      let match_body = match_ ~ctx1 body_motif in
      fun expr ~env ~ctx ->
        match expr.pexp_desc with
        | Pexp_function (params, None, Pfunction_body body) ->
            match_params params ~env ~ctx && match_body body ~env ~ctx
        | _ -> false)
  | Pexp_extension ({ txt = "acc"; _ }, _) ->
      fun expr ~env ~ctx:_ ->
        Option.is_none !env.xrest
        &&
        (env := { !env with xrest = Some (Expr expr) };
         true)
  | Pexp_extension
      ( { txt = "X"; _ }
      , PStr
          [ { pstr_desc =
                Pstr_eval ({ pexp_desc = Pexp_ident { txt = Lident motif; _ }; _ }, _)
            ; _
            }
          ] ) ->
      match_var ~x:true motif (fun v -> Expr v)
  | Pexp_extension
      ({ txt = "repeat"; _ }, PStr [ { pstr_desc = Pstr_eval (motif, _); _ } ]) ->
      let base, repeated =
        let base_motif = ref None in
        let super = Ast_mapper.default_mapper in
        let mapper =
          { super with
            expr =
              (fun self expr ->
                match expr.pexp_desc with
                | Pexp_extension
                    ({ txt = "acc"; _ }, PStr [ { pstr_desc = Pstr_eval (b, _); _ } ]) ->
                    base_motif := Some b;
                    expr
                | _ -> super.expr self expr)
          }
        in
        let repeated = mapper.expr mapper motif in
        (Option.value_exn !base_motif, repeated)
      in
      let match_repeated = match_ ~ctx1 repeated in
      let match_base = match_ ~ctx1 base in
      let rec loop xvars expr ~env ~ctx =
        let env_before = !env in
        (if
           env := { !env with xbindings = Map.empty (module String); xrest = None };
           match_repeated expr ~env ~ctx
         then
           match !env.xrest with
           | Some (Expr expr) -> loop (!env.xbindings :: xvars) expr ~env ~ctx
           | rest -> raise_s [%sexp "rest is not expr", (rest : value option)]
         else false)
        ||
        (env := env_before;
         match_base expr ~env ~ctx
         && match_count ~env (List.length xvars + 1)
         && List.for_alli xvars ~f:(fun i map ->
                let i = i + 2 in
                Map.for_alli map ~f:(fun ~key ~data ->
                    match_var_snd_stage ~env (key ^ Int.to_string i) data)))
      in
      loop []
  | Pexp_extension ({ txt = "repeat2"; _ }, PStr [ { pstr_desc = Pstr_eval (e, _); _ } ])
    -> (
      match e.pexp_desc with
      | Pexp_function
          ( [ { pparam_desc =
                  Pparam_val
                    (Nolabel, None, { ppat_desc = Ppat_var { txt = var_base; _ }; _ })
              ; _
              }
            ]
          , None
          , Pfunction_body body_motif ) -> (
          let match_params (params : P.expr_function_param list) ~env ~ctx:_ =
            List.for_alli params ~f:(fun i param ->
                match param.pparam_desc with
                | Pparam_val (Nolabel, None, pat) ->
                    match_var_snd_stage ~env (var_base ^ Int.to_string (i + 1)) (Pat pat)
                | Pparam_newtype _ | Pparam_val _ -> false)
            && match_count ~env (List.length params)
          in
          let match_body = match_ ~ctx1 body_motif in
          fun expr ~env ~ctx ->
            match expr.pexp_desc with
            | Pexp_function (params, None, Pfunction_body body) ->
                match_params params ~env ~ctx && match_body body ~env ~ctx
            | _ -> false)
      | _ -> unsupported_motif motif.pexp_loc)
  | Pexp_extension (motif_name, motif_payload) -> (
      let s_payload = match_payload ~loc:motif.pexp_loc ~ctx1 motif_payload in
      match motif_name.txt with
      | "move_def" -> (
          ctx1.need_type_index := true;
          fun expr ~env ~ctx ->
            (* The use of whole_ast is problematic because if the fraction of ast we're
             getting from whole_ast has been modified by the current transform (for
             instance, by a match of [%move_def] inside it), we're losing these
             modifications (except definition deletions, in the case of [%move_def]).
             Ideally, we'd refrain from performing these kind of nested rewrites.  This
             is unlike what happens without [%move_def], which motifs match on the AST
             being rewritten. *)
            match ctx.whole_ast with
            | None -> false
            | Some whole_ast -> (
                match expr.pexp_desc with
                | Pexp_ident { txt = _; _ } -> (
                    match Lazy.force ctx.type_index with
                    | None ->
                        if !log || debug.all then print_s [%sexp "missing type index"];
                        false
                    | Some index -> (
                        match
                          Build.Type_index.exp index (Conv.location' expr.pexp_loc)
                        with
                        | [] ->
                            if !log || debug.all
                            then print_s [%sexp (expr : expression), "no type"];
                            false
                        | texpr :: _ -> (
                            match texpr.exp_desc with
                            | Texp_ident (_, _, vd) -> (
                                let uid = vd.val_uid in
                                match locate_def ~index whole_ast uid with
                                | None -> false
                                | Some def ->
                                    let def =
                                      match force ctx.new_base_env with
                                      | None -> def
                                      | Some (index, new_base_env) -> (
                                          match Build.Type_index.find index Exp def with
                                          | [] -> def
                                          | texp :: _ ->
                                              let rebased_env =
                                                Uast.Env_summary.rebase'
                                                  ~old_base:texp.exp_env
                                                  ~new_base:new_base_env
                                                |> __.next
                                              in
                                              let mapper =
                                                Requalify.requalify_deeply (fun e ->
                                                    match
                                                      Build.Type_index.find index Exp e
                                                    with
                                                    | [] -> None
                                                    | z :: _ ->
                                                        let orig_env =
                                                          Envaux.env_of_only_summary
                                                            (Build.Type_index.env Exp z)
                                                        in
                                                        Some
                                                          ( orig_env
                                                          , new_base_env
                                                          , rebased_env orig_env ))
                                              in
                                              mapper.expr mapper def)
                                    in
                                    env :=
                                      { !env with
                                        nodes_to_remove = uid :: !env.nodes_to_remove
                                      };
                                    s_payload
                                      (P.PStr [ Ast_helper.Str.eval def ])
                                      ~env ~ctx)
                            | _ -> assert false)))
                | _ -> false))
      | _ -> (
          fun expr ~env ~ctx ->
            match expr.pexp_desc with
            | Pexp_extension (name, payload) ->
                name.txt =: motif_name.txt && s_payload payload ~env ~ctx
            | _ -> false))
  | _ -> unsupported_motif motif.pexp_loc

and match_fields ~ctx1 (mfields : (Longident.t Location.loc * _ * expression option) list)
    =
  let var_other = ref None in
  let s_named = ref (Map.empty (module Longident)) in
  List.iter mfields ~f:(fun (id, _, m) ->
      let m = Option.value_exn m in
      match id.txt with
      | Lident id'
        when (match m.pexp_desc with
             | Pexp_ident { txt = Lident id''; _ } when id'' =: id' -> true
             | _ -> false)
             && String.is_prefix id' ~prefix:"__etc" ->
          var_other := Some id'
      | _ -> s_named := Map.add_exn !s_named ~key:id.txt ~data:(id.txt, match_ ~ctx1 m));
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

and match_param ~ctx1 (p : P.expr_function_param) =
  match p.pparam_desc with
  | Pparam_newtype names -> unsupported_motif (List.hd_exn names).loc
  | Pparam_val (_, Some e, _) -> unsupported_motif e.pexp_loc
  | Pparam_val (arg_label_m, None, pat_m) -> (
      let match_p = match_pat ~ctx1 pat_m in
      fun (p : P.expr_function_param) ~env ~ctx ->
        match p.pparam_desc with
        | Pparam_newtype _ -> false
        | Pparam_val (_, Some _, _) -> false
        | Pparam_val (arg_label, None, pat) ->
            Arg_label.equal arg_label_m arg_label && match_p pat ~env ~ctx)

and match_pat ~ctx1 (p : pattern) =
  match p.ppat_desc with
  | Ppat_var v_motif when String.is_prefix v_motif.txt ~prefix:"__" ->
      match_var v_motif.txt (fun p -> Pat p)
  | Ppat_construct (id_motif, ((None | Some ([], _)) as motif_opt)) -> (
      let sopt = match_option (match_pat ~ctx1) (Option.map ~f:snd motif_opt) in
      fun pat ~env ~ctx ->
        match pat.ppat_desc with
        | Ppat_construct (id, ((None | Some ([], _)) as popt)) ->
            Fmast.Longident.compare id.txt id_motif.txt = 0
            && sopt (Option.map ~f:snd popt) ~env ~ctx
        | _ -> false)
  | Ppat_variant (v, p2) -> (
      let s_label =
        if String.is_suffix v.txt.txt ~suffix:"__"
        then match_var v.txt.txt (fun t -> Variant t)
        else fun v' ~env:_ ~ctx:_ -> v.txt.txt =: v'
      in
      let s_payload = match_option (match_pat ~ctx1) p2 in
      fun p ~env ~ctx ->
        match p.ppat_desc with
        | Ppat_variant (v, p2) -> s_label v.txt.txt ~env ~ctx && s_payload p2 ~env ~ctx
        | _ -> false)
  | _ -> unsupported_motif p.ppat_loc

and match_args ~ctx1 (margs : (arg_label * expression) list) =
  let var_other = ref None in
  let s_named = ref (Map.empty (module String)) in
  let s_anon = Queue.create () in
  List.iter margs ~f:(fun (arg_label, motif) ->
      match arg_label with
      | Nolabel -> (
          match motif.pexp_desc with
          | Pexp_ident { txt = Lident v; _ } when String.is_prefix v ~prefix:"__etc" ->
              var_other := Some v
          | _ -> Queue.enqueue s_anon (match_ ~ctx1 motif))
      | _ ->
          s_named :=
            Map.add_exn !s_named
              ~key:(Arg_label.to_string arg_label)
              ~data:(arg_label, match_ ~ctx1 motif));
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
    | None -> List.is_empty !s_anon
    | Some var_other -> (
        match Map.add !env.bindings ~key:var_other ~data:(Args (List.rev !others)) with
        | `Ok map ->
            env := { !env with bindings = map };
            true
        | `Duplicate -> false)

and match_payload ~ctx1 ~loc (motif : P.payload) =
  match motif with
  | PStr m -> (
      let sm = match_structure ~ctx1 ~loc m in
      fun s ~env ~ctx -> match s with PStr si -> sm si ~env ~ctx | _ -> false)
  | _ -> unsupported_motif loc

and match_structure ~loc ~ctx1 (stritems : structure_item list) =
  match_list (match_structure_item ~loc ~ctx1) stritems Option.some

and match_structure_item ~loc ~ctx1 (motif : structure_item) =
  match motif.pstr_desc with
  | Pstr_eval (motif1, _) -> (
      let s1 = match_ ~ctx1 motif1 in
      fun (s : P.structure_item) ~env ~ctx ->
        match s.pstr_desc with Pstr_eval (e, _) -> s1 e ~env ~ctx | _ -> false)
  | _ -> unsupported_motif loc

let get_count bindings =
  match Map.find bindings "__count" with
  | Some
      (Expr { pexp_desc = Pexp_constant { pconst_desc = Pconst_integer (n, None); _ }; _ })
    ->
      Int.of_string n
  | _ -> failwith "no __count?"

let subst ~env =
  let super = Ast_mapper.default_mapper in
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
        | Ppat_var { txt = var; _ } when Map.mem env var ->
            env_pat ~loc:pat.ppat_loc env var
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
            | Pexp_extension
                ({ txt = "repeat2"; _ }, PStr [ { pstr_desc = Pstr_eval (e, _); _ } ]) ->
                Rep.repeat2_template e self ~count:(get_count env)
                  ~env_pat:(fun var -> env_pat ~loc:expr.pexp_loc env var)
                  ~env_exp:(fun var -> env_exp ~loc:expr.pexp_loc env var)
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
            | Pexp_apply (f, args) when Option.is_some (List.find_map args ~f:etc_arg) ->
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
          | Pexp_ident { txt = Lident var; _ } when Map.mem env var ->
              env_exp ~loc:expr.pexp_loc env var
          | _ -> expr)
  }

let replace (type a b e) ((vnode : (_, a, b, e) Fmast.Node.t), (v : a)) ~whole_ast
    ~type_index ~stage2 ~(repl : a) =
  let env =
    ref
      { bindings = Map.empty (module String)
      ; xbindings = Map.empty (module String)
      ; xrest = None
      ; nodes_to_remove = []
      }
  in
  if
    stage2 v ~env
      ~ctx:
        { type_index
        ; whole_ast
        ; new_base_env =
            lazy
              (match (vnode, v) with
              | Exp, (v : expression)
              | Binding_op, { pbop_exp = v; _ }
              | Value_binding, { pvb_body = Pfunction_body v; _ } -> (
                  match force type_index with
                  | None -> None
                  | Some type_index -> (
                      match Build.Type_index.find type_index Exp v with
                      | [] -> None
                      | z :: _ ->
                          Some
                            ( type_index
                            , Envaux.env_of_only_summary (Build.Type_index.env Exp z) )))
              | _ -> None)
        }
  then
    let repl : a =
      match vnode with
      | Exp -> preserve_loc_to_preserve_comment_pos_expr ~from:v repl
      | _ ->
          preserve_loc_to_preserve_comment_pos (Fmast.Node.meth vnode)
            ~from:(Fmast.Node.loc vnode v) repl
    in
    Some (Fmast.Node.map vnode (subst ~env:!env.bindings) repl, !env.nodes_to_remove)
  else None

let rec match_type ~ctx1 (t : core_type) =
  match t.ptyp_desc with
  | Ptyp_constr ({ txt = Lident v; _ }, []) when String.is_prefix v ~prefix:"__" ->
      match_var v (fun t -> Typ t)
  | Ptyp_variant
      ([ { prf_desc = Rtag (label, has_no_payload, types); _ } ], closed_flag, None)
    when Bool.( = ) has_no_payload (List.is_empty types) -> (
      let s_label =
        if String.is_suffix label.txt.txt ~suffix:"__"
        then match_var label.txt.txt (fun t -> Variant t)
        else fun label' ~env:_ ~ctx:_ -> label.txt.txt =: label'
      in
      let s_typ =
        match types with
        | [] ->
            fun (has_no_payload, types) ~ctx:_ ~env:_ ->
              has_no_payload && List.is_empty types
        | m_typ :: _ -> (
            let s_typ = match_type ~ctx1 m_typ in
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

let match_value_binding ~ctx1
    ({ pvb_pat
     ; pvb_args = _
     ; pvb_is_pun = _
     ; pvb_body
     ; pvb_constraint
     ; pvb_attributes = _
     ; pvb_loc
     } :
      P.value_binding) =
  match pvb_constraint with
  | Some _ -> unsupported_motif pvb_loc
  | None -> (
      let s_pat = match_pat ~ctx1 pvb_pat in
      let s_expr =
        match pvb_body with
        | Pfunction_cases _ -> assert false
        | Pfunction_body expr -> match_ ~ctx1 expr
      in
      fun (vb : P.value_binding) ~env ~ctx ->
        Option.is_none vb.pvb_constraint
        && s_pat vb.pvb_pat ~env ~ctx
        &&
        match vb.pvb_body with
        | Pfunction_cases _ -> assert false
        | Pfunction_body body -> s_expr body ~env ~ctx)

let split_bop str = (String.prefix str 3, String.drop_prefix str 3)

let match_binding_op ~ctx1
    ({ pbop_op
     ; pbop_pat
     ; pbop_args = _
     ; pbop_is_pun = _
     ; pbop_typ = _
     ; pbop_exp
     ; pbop_loc = _
     } :
      P.binding_op) =
  (* turn let+/and+ into + *)
  let m_op = split_bop pbop_op.txt in
  let s_pat = match_pat ~ctx1 pbop_pat in
  let s_expr = match_ ~ctx1 pbop_exp in
  fun (op : P.binding_op) ~env ~ctx ->
    snd m_op =: snd (split_bop op.pbop_op.txt)
    && s_pat op.pbop_pat ~env ~ctx
    && s_expr op.pbop_exp ~env ~ctx

let compile_motif ~ctx1 motif =
  let parse ast motif =
    let file_path = "command line param" in
    (Fmast.parse_with_ocamlformat ~conf:Ocamlformat_lib.Conf.default ast
       ~input_name:file_path motif)
      .ast
    |> Ocamlformat_lib.Extended_ast.map ast drop_concrete_syntax_constructs
    |> Ocamlformat_lib.Extended_ast.map ast remove_attributes
  in
  match String.lsplit2 motif ~on:':' with
  | Some ("binding", motif) -> (
      match parse Structure motif with
      | [ { pstr_desc =
              Pstr_eval
                ( { pexp_desc =
                      Pexp_let ({ pvbs_rec = Nonrecursive; pvbs_bindings = [ vb ] }, _, _)
                  ; _
                  }
                , _ )
          ; _
          }
        ] ->
          `Binding (match_value_binding ~ctx1 vb)
      | [ { pstr_desc =
              Pstr_eval
                ( { pexp_desc =
                      Pexp_letop { let_ = binding_op; ands = []; body = _; loc_in = _ }
                  ; _
                  }
                , _ )
          ; _
          }
        ] ->
          `Binding_op (match_binding_op ~ctx1 binding_op)
      | stri :: _ -> unsupported_motif stri.pstr_loc
      | [] -> unsupported_motif Location.none)
  | Some ("type", motif) ->
      let typ = parse Core_type motif in
      `Type (match_type ~ctx1 typ)
  | _ ->
      let motif = parse Expression motif in
      let motif_fm = Rep.infer motif in
      if
        debug.replace_repetitions
        && Sexp.( <> ) [%sexp (motif : expression)] [%sexp (motif_fm : expression)]
      then
        print_s
          [%sexp
            `inferred_repetition, (motif : expression), "->", (motif_fm : expression)];
      `Expr (match_ ~ctx1 motif_fm)

let parse_template ~fmconf stage2 repl =
  let repl ?(infer_repetition = Fn.id) wrap unwrap kind =
    match !repl with
    | `Forced v -> unwrap v
    | `Unforced repl_str ->
        let v =
          (Fmast.parse_with_ocamlformat kind ~conf:fmconf ~input_name:migrate_filename_gen
             (* important for comment placement, among which
                preserve_loc_to_preserve_comment_pos to work *)
             repl_str)
            .ast
          |> infer_repetition
          |> Ocamlformat_lib.Extended_ast.map kind
               Transform_migration.internalize_attribute_mapper
        in
        repl := `Forced (wrap v);
        v
  in
  match stage2 with
  | `Expr stage2 ->
      `Expr
        ( stage2
        , repl ~infer_repetition:Rep.infer (`Expr __)
            (function `Expr v -> v | _ -> assert false)
            Expression )
  | `Type stage2 ->
      `Type
        (stage2, repl (`Type __) (function `Type v -> v | _ -> assert false) Core_type)
  | (`Binding _ | `Binding_op _) as stage2 -> (
      match
        (stage2, repl (`Str __) (function `Str v -> v | _ -> assert false) Structure)
      with
      | `Binding stage2, stri -> (
          match stri with
          | [ { pstr_desc =
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
          | stri :: _ -> unsupported_motif stri.pstr_loc
          | [] -> unsupported_motif Location.none)
      | `Binding_op stage2, stri -> (
          match stri with
          | [ { pstr_desc =
                  Pstr_eval
                    ({ pexp_desc = Pexp_letop { let_ = binding_op; ands = []; _ }; _ }, _)
              ; _
              }
            ] ->
              `Binding_op (stage2, binding_op)
          | stri :: _ -> unsupported_motif stri.pstr_loc
          | _ -> unsupported_motif Location.none))

let substr_split t ~on:substr =
  let substr = String.Search_pattern.create substr in
  String.Search_pattern.split_on substr t

let split_motif_repl str =
  match substr_split str ~on:"///" with
  | [ s1; s2 ] -> (s1, s2)
  | _ ->
      raise_s
        [%sexp
          "unexpected exactly one \"///\" in argument"
        , (str : string)
        , "to separate MOTIF and REPL"]

let find_motif_repl si =
  match structure_item_attributes si with
  | None -> None
  | Some (attrs, _) -> (
      match
        List.find_map (attrs.attrs_before @ attrs.attrs_after) ~f:(fun attr ->
            if attr.attr_name.txt =: "migrate_test.replace_expr"
            then
              match attr.attr_payload with
              | PStr
                  [ { pstr_desc =
                        Pstr_eval
                          ( { pexp_desc =
                                Pexp_constant { pconst_desc = Pconst_string (s, _, _); _ }
                            ; _
                            }
                          , _ )
                    ; _
                    }
                  ] ->
                  Some s
              | _ -> failwith "bad migrate_test.replace_expr attr"
            else None)
      with
      | None -> None
      | Some payload -> Some (split_motif_repl payload))

let run ~(listing : Build.Listing.t) motif_and_repls () =
  ((* set up load paths to be able to type the motifs *)
   let load_path_dirs =
     Hashtbl.to_alist listing.all_load_paths
     |> List.sort ~compare:(fun (d1, _) (d2, _) -> Cwdpath.compare d1 d2)
     |> List.map ~f:(fun (_, (lazy load_path)) -> load_path)
   in
   Load_path.init ~auto_include:Load_path.no_auto_include ~visible:[] ~hidden:[];
   List.iter load_path_dirs ~f:Load_path.append_dir);
  let may_need_type_index_ref = ref false in
  let stage2_and_repls =
    List.map motif_and_repls ~f:(fun (motif, repl) ->
        let repl = ref (`Unforced repl) in
        (compile_motif ~ctx1:{ need_type_index = may_need_type_index_ref } motif, repl))
  in
  fun ~fmconf ~type_index ~source_path ~input_name_matching_compilation_command ->
    let stage2_and_repls =
      ref
        (List.map stage2_and_repls ~f:(fun (stage2, repl) ->
             parse_template ~fmconf stage2 repl))
    in
    let input_name_matching_compilation_command =
      if !may_need_type_index_ref
      then force input_name_matching_compilation_command
      else None
    in
    process_file ~fmconf ~source_path ~input_name_matching_compilation_command
      { f =
          (fun (type a) changed_something file_type (structure : a) ->
            let should_act_in_test = ref false in
            let whole_ast =
              ref (if in_test then None else File_type.structure file_type structure)
            in
            let all_nodes_to_remove = Queue.create () in
            let super = Ast_mapper.default_mapper in
            let self =
              { super with
                typ =
                  (fun self ty ->
                    let ty = super.typ self ty in
                    match
                      List.find_map !stage2_and_repls ~f:(function
                        | `Type (stage2, repl) ->
                            replace (Typ, ty) ~whole_ast:!whole_ast ~type_index ~stage2
                              ~repl
                        | _ -> None)
                    with
                    | None -> ty
                    | Some (ty, nodes_to_remove) ->
                        Queue.enqueue_all all_nodes_to_remove nodes_to_remove;
                        changed_something := true;
                        ty)
              ; type_declaration =
                  (fun self v ->
                    (* prevents recursion into type params, which had a sensible
                       representation of string option until
                       https://github.com/ocaml/ocaml/issues/5584, which turned into
                       core_type :/. This allows type replacements to not apply to type
                       parameters, as they would likely creates asts that have no
                       syntax if any rewrite happens. *)
                    { (super.type_declaration self { v with ptype_params = [] }) with
                      ptype_params = v.ptype_params
                    })
              ; value_binding =
                  (fun self vb ->
                    let vb = super.value_binding self vb in
                    match
                      List.find_map !stage2_and_repls ~f:(function
                        | `Binding (stage2, repl) ->
                            replace (Value_binding, vb) ~whole_ast:!whole_ast ~type_index
                              ~stage2 ~repl
                        | _ -> None)
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
                      List.find_map !stage2_and_repls ~f:(function
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
                            replace (Binding_op, bop) ~whole_ast:!whole_ast ~type_index
                              ~stage2 ~repl
                        | _ -> None)
                    with
                    | None -> bop
                    | Some (bop, nodes_to_remove) ->
                        Queue.enqueue_all all_nodes_to_remove nodes_to_remove;
                        changed_something := true;
                        bop)
              ; expr =
                  with_log (fun self expr ->
                      let expr = super.expr self expr in
                      match
                        List.find_map !stage2_and_repls ~f:(function
                          | `Expr (stage2, repl) ->
                              replace (Exp, expr) ~whole_ast:!whole_ast ~type_index
                                ~stage2 ~repl
                          | _ -> None)
                      with
                      | None -> expr
                      | Some (expr, nodes_to_remove) ->
                          Queue.enqueue_all all_nodes_to_remove nodes_to_remove;
                          changed_something := true;
                          expr)
              ; structure_item =
                  (let match_attr = __ =: "migrate_test.replace" in
                   let filter_attr = String.is_prefix ~prefix:"migrate_test.replace" in
                   let f =
                     update_migrate_test_payload ~match_attr ~filter_attr
                       ~state:should_act_in_test
                       { super with
                         structure_item =
                           (fun self si ->
                             if !should_act_in_test && Option.is_none !whole_ast
                             then
                               (* In scope, narrow the scope of [%move_def] to only
                                what's under a [@@migrate_test.replace] annotation,
                                otherwise the changes "leak" out of the
                                [@@migrate_test.replace] attribute. *)
                               Ref.set_temporarily whole_ast (Some [ si ]) ~f:(fun () ->
                                   let si = super.structure_item self si in
                                   match
                                     drop_defs Impl ~type_index [ si ]
                                       (Queue.to_list all_nodes_to_remove)
                                   with
                                   | [ si ] ->
                                       Queue.clear all_nodes_to_remove;
                                       si
                                   | _ -> assert false)
                             else super.structure_item self si)
                       }
                       ~changed_something
                     |> __.next
                   in
                   fun self si ->
                     match find_motif_repl si with
                     | None -> f self si
                     | Some (motif, repl) ->
                         Ref.set_temporarily stage2_and_repls
                           [ compile_motif
                               ~ctx1:{ need_type_index = may_need_type_index_ref }
                               motif
                             |> parse_template ~fmconf __ (ref (`Unforced repl))
                           ]
                           ~f:(fun () -> f self si))
              }
            in
            let structure = File_type.map file_type self structure in
            drop_defs file_type ~type_index structure (Queue.to_list all_nodes_to_remove))
      }
