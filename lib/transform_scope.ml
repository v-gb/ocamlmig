open Base
open! Stdio
module Filename = Stdlib.Filename
module Sys = Stdlib.Sys
module Printf = Stdlib.Printf
module Format = Stdlib.Format
open Common
open Transform_common
open! Ocamlformat_ocaml_common
open Ocamlformat_parser_extended
module P = Parsetree
open Fmast

let wrap_log lazy_sexp f =
  if !log
  then (
    print_s (force lazy_sexp);
    let r = f () in
    print_s [%sexp "done"];
    r)
  else f ()

let rec normal_form_of_use ~artifacts env
    (comp_unit, (id : Uast.Longident.t Uast.Location.loc)) =
  wrap_log
    (lazy [%sexp "normal_form_of_use", ((comp_unit, id.txt) : string * Uast.Longident.t)])
    (fun () ->
      match Build.Artifacts.shape_from_occurrence artifacts (comp_unit, id) with
      | None ->
          (* stdlib built without occurrence! *)
          if !log
          then
            print_s
              [%sexp
                "no shape for occurrence"
              , ((comp_unit, id) : string * Uast.Longident.t Uast.Location.loc)];
          None
      | Some decl_result -> normal_form_of_decl_result ~artifacts env decl_result)

and normal_form_of_decl_result ~artifacts env decl_result =
  wrap_log
    (lazy
      [%sexp
        "normal_form_of_decl_result"
      , (decl_result : (Uast.Shape.Uid.t * _ option, string Lazy.t) Result.t)
      , (artifacts : Build.Artifacts.t)])
    (fun () ->
      match decl_result with
      | Error s ->
          if !log
          then print_s [%sexp "normal_form_of_decl_result: error", (force s : string)];
          None
      | Ok (uid, decl_opt) ->
          Option.bind decl_opt ~f:(normal_form_of_decl env ~artifacts)
          |> Option.value ~default:(`Uid uid)
          |> Some __)

and normal_form_of_decl ~artifacts env decl =
  wrap_log
    (lazy [%sexp "normal_form_of_decl"])
    (fun () ->
      match (decl : Typedtree.item_declaration) with
      | Value vd -> Some (`Prim (env, vd))
      | Value_binding vb -> (
          match (vb.vb_pat, vb.vb_expr) with
          | ( { pat_desc = Tpat_var (_, _, uid); _ }
            , { exp_desc = Texp_ident (_, id2, _); _ } ) ->
              (* Follow through value aliases. Shapes only follow module aliases,
                 or aliases created by module inclusion and coercions. *)
              Option.bind (Build.comp_unit_of_uid uid) ~f:(fun comp_unit ->
                  normal_form_of_use ~artifacts vb.vb_expr.exp_env (comp_unit, id2))
          | _ ->
              if !log then print_s [%sexp "not an alias", (vb.vb_loc : Uast.Location.t)];
              None)
      | _ -> None)

let normal_form_of_path ~artifacts env (path : Uast.Path.t) (vd : Types.value_description)
    =
  wrap_log
    (lazy [%sexp "normal_form_of_path", (path : Uast.Path.t)])
    (fun () ->
      Option.bind (Build.comp_unit_of_uid vd.val_uid) ~f:(fun comp_unit ->
          let shape = Env.shape_of_path env path ~namespace:Value in
          Build.Artifacts.decl_from_following_shape artifacts (comp_unit, shape)
          |> normal_form_of_decl_result ~artifacts env)
      |> Option.value ~default:(`Uid vd.val_uid))

let decl_is_deprecated ~artifacts ~comp_unit ~id_for_logging:id
    (vd : Types.value_description) =
  let uid = vd.val_uid in
  match Build.comp_unit_of_uid uid with
  | None ->
      if !log then print_s [%sexp (id : Longident.t), "no compilation unit"];
      false
  | Some comp_unit' -> (
      let source = if comp_unit' =: comp_unit then `impl else `intf in
      match Build.Artifacts.sigitem_from_def_uid artifacts (uid, source) with
      | None ->
          if !log
          then print_s [%sexp (id : Longident.t), "no decl", (source : [ `impl | `intf ])];
          false
      | Some sigitem ->
          let attributes = Uast.decl_or_sigitem_attributes sigitem in
          let b =
            List.exists attributes ~f:(fun attr ->
                attr.attr_name.txt =: "deprecated"
                || attr.attr_name.txt =: "ocaml.deprecated")
          in
          if !log then print_s [%sexp (id : Longident.t), "has @deprecated", (b : bool)];
          b)

let merely_aliased ~artifacts
    ((path, vd, env) : Uast.Path.t * Types.value_description * Uast.env)
    ((path', vd', env') : Uast.Path.t * Types.value_description * Uast.env) =
  (* In the general case, there are multiple problems with this code:
     - even if the values on both sides are the same, their type might not be.
       Example: all the %identity functions, or compare in Base vs Stdlib,
       or Bytes.unsafe_to_string in Base vs Stdlib. This is partly handled for
       primitives below. Although even there, we check that the new function is
       more general than the old one or something. The new function being
       more restrictive can cause type errors, but the new function being more
       permissive can cause type inference failure (just imagine changing a
       compare : 'a -> 'a -> int, used as compare M.A A, into
       val compare : 'a -> 'b -> int: you'd get unbound constructor A on the second A),
       arbitrarily far away from the code change. In these case, using the
       conservative option would at least be possible. But perhaps we should
       check that both values have equivalent types, or check that the new value
       gits in the new type and add a type annotation to remove the extra generality.
     - even if the values on both sides are the same, the new one might
       be deprecated. Example: Base.open_in. This should be checked before calling this.
     - two values coming from the same source code location doesn't imply that they
       are equal. In practice, shapes only talk about modules and not core values,
       but still, we might be wrongly considering that M1.x and M2.x are aliased
       in this example:
       module F(X : sig val x : int end) = X
       module M1 = F(struct let x = 1 end)
       module M2 = F(struct let x = 1 end)
       Same problem with extensible sum type constructors, since their declaration
       is generative. *)
  let nf, nf' =
    ( normal_form_of_path ~artifacts env path vd
    , normal_form_of_path ~artifacts env' path' vd' )
  in
  (if !log
   then
     let show path (vd : Types.value_description) nf =
       [%sexp
         (path : Uast.Path.t)
       , (vd.val_uid : Uast.Shape.Uid.t)
       , "->"
       , (nf : [ `Uid of Uast.Shape.Uid.t | `Prim of _ ])]
     in
     print_s [%sexp (show path vd nf : Sexp.t), "vs", (show path' vd' nf' : Sexp.t)]);
  match (nf, nf') with
  | `Uid uid, `Uid uid' -> Shape.Uid.equal uid uid'
  | `Uid _, _ | _, `Uid _ -> false
  | `Prim (env, p), `Prim (_env', p') ->
      (* Same name and same primitive probably implies same type, but not always, with
         (=) and compare in Stdlib vs Base for instance. Maybe the more general check
         would be to check that the new type would accept the specific type of the call
         site. *)
      p.val_name.txt =: p'.val_name.txt
      && Uast.match_typ ~env vd.val_type ~user_type:vd'.val_type

type t =
  | Unopen of
      { name : string
      ; conservative : bool
      }
  | Open of
      { name : string
      ; bang : bool
      ; conservative : bool
      }
  | Unqualify of string list

let qualify_for_unopen ~changed_something ~artifacts ~type_index
    ~(cmt_infos : Cmt_format.cmt_infos) structure root ~conservative =
  (* It would be more accurate to check if the root name is unbound at every call site,
     and if not, introduce a name like Root_unshadowed to be used instead. *)
  let comp_unit = cmt_infos.cmt_modname in
  let rec maybe_reroot root (lid : Longident.t) (path : Path.t) : Longident.t option =
    match (lid, path) with
    | Lident s, Pdot (rest, s')
      when (* We look for Root.foo and Global.Root.foo, because the Global might be
              introduced by dune's aliases. Maybe we should simply chop off a suffix
              of [path] instead? *)
           match rest with
           | Pident root' -> Ident.global root' && Ident.name root' =: root
           | Pdot (Pident global, root') -> Ident.global global && root' =: root
           | _ -> false ->
        assert (s =: s');
        Some (Ldot (Lident root, s))
    | Ldot (lid1, s1), Pdot (path1, s2) -> (
        assert (s1 =: s2);
        match maybe_reroot root lid1 path1 with
        | None -> None
        | Some lid -> Some (Ldot (lid, s1)))
    | Lapply (lid1, lid2), Papply (path1, path2) ->
        let lidl = maybe_reroot root lid1 path1 in
        let lidr = maybe_reroot root lid2 path2 in
        if Option.is_none lidl && Option.is_none lidr
        then None
        else
          Some (Lapply (Option.value lidl ~default:lid1, Option.value lidr ~default:lid2))
    | _ -> None
  in
  let initial_env = Envaux.env_of_only_summary cmt_infos.cmt_initial_env in
  let super = Ast_mapper.default_mapper in
  let is_root (lid : Longident.t) = match lid with Lident s -> s =: root | _ -> false in
  (* We'd need to also names in all other scopes. One tricky bit is that deriving creates
     value names from type names (foo -> sexp_of_foo), so we presumably have to replace
     int by Base.int, at least depending on context. When we try to tackle this, perhaps
     the way to do it is to look for an expression at the location of the type
     constructor (I'd expect [%sexp_of: int] to generate sexp_of_int at the location
     of int). *)
  let should_act_in_test = ref false in
  let self =
    { super with
      expr =
        with_log (fun self expr ->
            if in_test && not !should_act_in_test
            then super.expr self expr
            else
              match expr.pexp_desc with
              | Pexp_ident id -> (
                  match
                    Build.Type_index.expr type_index (Conv.location' expr.pexp_loc)
                  with
                  | [] ->
                      if !log then print_s [%sexp (id.txt : Longident.t), "missing type"];
                      expr
                  | texpr :: _ -> (
                      (* When [f] is transformed by the typechecker in [f ~foo:None] to
                         fill in unsupplied optional argument, the loc of the location and
                         argument seem to be the loc of f, and not even marked as ghost?
                         So we get multiple expressions at the same location, and f itself
                         seems to be first in this case. *)
                      let env = Envaux.env_of_only_summary texpr.exp_env in
                      match Env.find_value_by_name (Conv.longident' id.txt) env with
                      | exception Stdlib.Not_found ->
                          (* happens with __, because of ppx_partial *)
                          expr
                      | path, vd -> (
                          match maybe_reroot root id.txt path with
                          | None -> expr
                          | Some new_id ->
                              let merely_aliased =
                                match
                                  Env.find_value_by_name (Conv.longident' id.txt)
                                    initial_env
                                with
                                | exception Stdlib.Not_found -> false
                                | path', vd' ->
                                    (not conservative)
                                    && (not
                                          (decl_is_deprecated ~id_for_logging:id.txt
                                             ~artifacts ~comp_unit vd'))
                                    && merely_aliased ~artifacts (path, vd, env)
                                         (path', vd', env)
                              in
                              if merely_aliased
                              then expr
                              else (
                                changed_something := true;
                                { expr with
                                  pexp_desc = Pexp_ident { id with txt = new_id }
                                ; pexp_attributes =
                                    Sattr.touched.build ~loc:!Ast_helper.default_loc ()
                                    :: expr.pexp_attributes
                                }))))
              | Pexp_open (id, e)
              | Pexp_letopen ({ popen_expr = { pmod_desc = Pmod_ident id; _ }; _ }, e)
                when is_root id.txt ->
                  changed_something := true;
                  self.expr self e
              | _ -> super.expr self expr)
    ; structure_item =
        update_migrate_test_payload ~match_attr:(__ =: "migrate_test.unopen")
          ~state:should_act_in_test ~changed_something super
    ; structure =
        (fun self sis ->
          List.filter sis ~f:(fun si ->
              match si.pstr_desc with
              | Pstr_open { popen_expr = { pmod_desc = Pmod_ident id; _ }; _ }
                when is_root id.txt && ((not in_test) || !should_act_in_test) ->
                  changed_something := true;
                  false
              | _ -> true)
          |> super.structure self)
    }
  in
  self.structure self structure

let qualify_for_open ~changed_something ~artifacts ~type_index
    ~(cmt_infos : Cmt_format.cmt_infos) structure root ~bang ~conservative =
  let comp_unit = cmt_infos.cmt_modname in
  let initial_env = Envaux.env_of_only_summary cmt_infos.cmt_initial_env in
  let sum_prepend_root =
    let path =
      match Env.find_module_by_name (Lident root) initial_env with
      | exception Stdlib.Not_found ->
          raise_s
            [%sexp
              "module to open is missing from search path", ("module", (root : string))]
      | path, _ -> path
    in
    let initial_env_summary_length =
      Uast.Env_summary.length (Env.summary cmt_infos.cmt_initial_env)
    in
    fun sum ->
      Uast.Env_summary.at_exn sum
        (Uast.Env_summary.length sum - initial_env_summary_length)
        (fun sum -> Env_open (sum, path))
  in
  let super = Ast_mapper.default_mapper in
  let prepended_env_of_only_summary env =
    Env.env_of_only_summary
      (fun sum subst -> Envaux.env_from_summary (sum_prepend_root sum) subst)
      env
  in
  let rec ident_of_path : Path.t -> Longident.t = function
    | Pident ident -> Lident (Ident.name ident)
    | Pdot (p, s) -> Ldot (ident_of_path p, s)
    | Papply (p1, p2) -> Lapply (ident_of_path p1, ident_of_path p2)
    | Pextra_ty _ -> raise Stdlib.Not_found
  in
  let update_constructor env (id : Longident.t Location.loc) =
    let prepended_env = prepended_env_of_only_summary env in
    let env = Envaux.env_of_only_summary env in
    let captured_by_open =
      match Env.find_constructor_by_name (Conv.longident' id.txt) env with
      | exception Stdlib.Not_found -> None
      | constructor_desc -> (
          match Env.find_constructor_by_name (Conv.longident' id.txt) prepended_env with
          | exception Stdlib.Not_found -> Some constructor_desc
          | constructor_desc' ->
              if Shape.Uid.equal constructor_desc.cstr_uid constructor_desc'.cstr_uid
              then None
              else Some constructor_desc)
    in
    match captured_by_open with
    | None -> None
    | Some constructor_desc -> (
        match constructor_desc.cstr_tag with
        | Cstr_constant _ | Cstr_block _ | Cstr_unboxed ->
            (* we should do these too, but haven't thought about it yet*)
            None
        | Cstr_extension _ -> (
            (* Our problem here is that Env.t doesn't expose the functionality
               to give us the qualified path to the constructor. I don't think
               we can take the path of the type because the type and the
               constructors may be defined separately (due to type aliases for regular
               sum types, and all the time for open sum types). So we're doing a hack
               that helps with Not_found, Exit etc.
            *)
            match Build.comp_unit_of_uid constructor_desc.cstr_uid with
            | Some "Stdlib" ->
                Some (Ldot (Lident "Stdlib", Longident.last id.txt) : Longident.t)
            | _ ->
                (* Almost all exceptions should be defined at toplevel, so we could try to
                   turn the compilation unit into Library.Module.Constructor. Not sure if
                   it's more annoying to do the wrong rewrites, or to fail to rewrite.
                   Probably best to leave it this way, and improve env.ml when the need
                   arises. *)
                if !log
                then
                  print_s
                    [%sexp
                      (id.txt : Longident.t)
                    , "want to rewrite constructor, but unable to figure out path to it"
                    , ~~(comp_unit : string)];
                None))
  in
  let should_act_in_test = ref false in
  let self =
    { super with
      pat =
        (fun self pat ->
          let pat = super.pat self pat in
          if in_test && not !should_act_in_test
          then pat
          else
            match pat.ppat_desc with
            | Ppat_construct (id, payload) -> (
                match Build.Type_index.pat type_index (Conv.location' pat.ppat_loc) with
                | [] -> pat
                | T tpat :: _ -> (
                    match update_constructor tpat.pat_env id with
                    | None -> pat
                    | Some new_id ->
                        changed_something := true;
                        { pat with
                          ppat_desc = Ppat_construct ({ id with txt = new_id }, payload)
                        ; ppat_attributes =
                            Sattr.touched.build ~loc:!Ast_helper.default_loc ()
                            :: pat.ppat_attributes
                        }))
            | _ -> pat)
    ; expr =
        with_log (fun self expr ->
            if in_test && not !should_act_in_test
            then super.expr self expr
            else
              match expr.pexp_desc with
              | Pexp_ident id -> (
                  match
                    Build.Type_index.expr type_index (Conv.location' expr.pexp_loc)
                  with
                  | [] ->
                      if !log then print_s [%sexp (id.txt : Longident.t), "missing type"];
                      expr
                  | texpr :: _ -> (
                      let env = texpr.exp_env in
                      let prepended_env = prepended_env_of_only_summary env in
                      let env = Envaux.env_of_only_summary env in
                      match Env.find_value_by_name (Conv.longident' id.txt) env with
                      | exception Stdlib.Not_found ->
                          (* happens with __, because of ppx_partial *)
                          expr
                      | path, vd -> (
                          let merely_aliased =
                            match
                              Env.find_value_by_name (Conv.longident' id.txt)
                                prepended_env
                            with
                            | exception Stdlib.Not_found -> false
                            | path', vd' ->
                                Shape.Uid.equal vd.val_uid vd'.val_uid
                                || (not conservative)
                                   && (not
                                         (decl_is_deprecated ~id_for_logging:id.txt
                                            ~artifacts ~comp_unit vd'))
                                   && merely_aliased ~artifacts (path, vd, env)
                                        (path', vd', prepended_env)
                          in
                          if merely_aliased
                          then expr
                          else
                            match ident_of_path path with
                            | exception Stdlib.Not_found -> expr
                            | new_id ->
                                changed_something := true;
                                { expr with
                                  pexp_desc = Pexp_ident { id with txt = new_id }
                                ; pexp_attributes =
                                    Sattr.touched.build ~loc:!Ast_helper.default_loc ()
                                    :: expr.pexp_attributes
                                })))
              | Pexp_construct (id, payload) ->
                  super.expr self
                    (match
                       Build.Type_index.expr type_index (Conv.location' expr.pexp_loc)
                     with
                    | [] -> expr
                    | texpr :: _ -> (
                        match update_constructor texpr.exp_env id with
                        | None -> expr
                        | Some new_id ->
                            changed_something := true;
                            { expr with
                              pexp_desc =
                                Pexp_construct ({ id with txt = new_id }, payload)
                            ; pexp_attributes =
                                Sattr.touched.build ~loc:!Ast_helper.default_loc ()
                                :: expr.pexp_attributes
                            }))
              | _ -> super.expr self expr)
    ; structure_item =
        update_migrate_test_payload ~match_attr:(__ =: "migrate_test.open")
          ~state:should_act_in_test ~changed_something super
    }
  in
  let structure' = self.structure self structure in
  if
    in_test
    ||
    (* not strictly necessary, but seems nice to be idempotent *)
    match structure' with
    | { pstr_desc =
          Pstr_open
            { popen_expr = { pmod_desc = Pmod_ident { txt = Lident mod_; _ }; _ }; _ }
      ; _
      }
      :: _ ->
        mod_ =: root
    | _ -> false
  then structure'
  else (
    changed_something := true;
    Ast_helper.with_default_loc (migrate_loc `Gen) (fun () ->
        let loc = !Ast_helper.default_loc in
        Ast_helper.Str.open_
          { popen_expr = Ast_helper.Mod.ident { txt = Lident root; loc }
          ; popen_override = (if bang then Override else Fresh)
          ; popen_loc = loc
          ; popen_attributes =
              { attrs_extension = None; attrs_before = []; attrs_after = [] }
          })
    :: structure')

let unqualify ~changed_something structure ~artifacts ~type_index
    ~(cmt_infos : Cmt_format.cmt_infos) roots =
  let _ = cmt_infos in
  let roots = Set.of_list (module String) roots in
  let maybe_deroot =
    let rec loop (lid : Longident.t) : Longident.t option =
      match lid with
      | Lident s -> if Set.mem roots s then None else Some lid
      | Ldot (lid1, s1) -> (
          match loop lid1 with
          | None -> Some (Lident s1)
          | Some lid1 -> Some (Ldot (lid1, s1)))
      | Lapply (lid1, lid2) ->
          Some
            (Lapply
               ( Option.value (loop lid1) ~default:lid1
               , Option.value (loop lid2) ~default:lid2 ))
    in
    fun lid ->
      match loop lid with
      | Some lid' when not (Fmast.Longident.compare lid lid' = 0) -> Some lid'
      | _ -> None
  in
  let super = Ast_mapper.default_mapper in
  let self =
    { super with
      expr =
        with_log (fun self expr ->
            match expr.pexp_desc with
            | Pexp_ident id -> (
                match Build.Type_index.expr type_index (Conv.location' expr.pexp_loc) with
                | [] ->
                    if !log then print_s [%sexp (id.txt : Longident.t), "missing type"];
                    expr
                | texpr :: _ -> (
                    let env = Envaux.env_of_only_summary texpr.exp_env in
                    match Env.find_value_by_name (Conv.longident' id.txt) env with
                    | exception Stdlib.Not_found ->
                        (* happens with __, because of ppx_partial *)
                        expr
                    | path, vd -> (
                        match maybe_deroot id.txt with
                        | None -> expr
                        | Some new_id ->
                            let merely_aliased =
                              match
                                Env.find_value_by_name (Conv.longident' new_id) env
                              with
                              | exception Stdlib.Not_found -> false
                              | path', vd' ->
                                  if !log
                                  then
                                    print_s
                                      [%sexp
                                        "right env?"
                                      , (new_id : Longident.t)
                                      , (path' : Uast.Path.t)
                                      , (vd'.val_uid : Uast.Shape.Uid.t)];
                                  merely_aliased ~artifacts (path, vd, env)
                                    (path', vd', env)
                            in
                            if !log
                            then
                              print_s
                                [%sexp
                                  (id.txt : Longident.t)
                                , (new_id : Longident.t)
                                , ~~(merely_aliased : bool)];
                            if merely_aliased
                            then (
                              changed_something := true;
                              { expr with
                                pexp_desc = Pexp_ident { id with txt = new_id }
                              ; pexp_attributes =
                                  Sattr.touched.build ~loc:!Ast_helper.default_loc ()
                                  :: expr.pexp_attributes
                              })
                            else expr)))
            | _ -> super.expr self expr)
    ; structure_item = update_migrate_test_payload ~changed_something super
    }
  in
  self.structure self structure

let run ~fmconf ~artifacts ~type_index ~cmt_infos transform ~source_path
    ~input_name_matching_compilation_command =
  process_file ~fmconf ~source_path ~input_name_matching_compilation_command
    (fun changed_something structure ->
      match transform with
      | Unopen { name; conservative } ->
          qualify_for_unopen ~changed_something structure ~artifacts ~type_index
            ~cmt_infos name ~conservative
      | Open { name; bang; conservative } ->
          qualify_for_open ~changed_something structure ~artifacts ~type_index ~cmt_infos
            name ~bang ~conservative
      | Unqualify names ->
          unqualify ~changed_something structure ~artifacts ~type_index ~cmt_infos names)
