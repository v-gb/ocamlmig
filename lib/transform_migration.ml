open Base
open Stdio
module Filename = Stdlib.Filename
module Sys = Stdlib.Sys
module Printf = Stdlib.Printf
module Format = Stdlib.Format
open Common
open Transform_common

type ctx =
  { has_ppx_partial : bool
  ; fmconf : Ocamlformat_lib.Conf.t
  ; module_migrations : bool
  }

type res = { libraries : string list }

open! Ocamlformat_ocaml_common
open Ocamlformat_parser_extended
module P = Parsetree
open Fmast

type 'repl gen_migrate_payload =
  { repl : 'repl
  ; libraries : string list
  }
[@@deriving sexp_of]

type migrate_payload = Fmast.expression gen_migrate_payload [@@deriving sexp_of]

let payload_attribute ~attrs { repl; libraries } =
  match (repl, libraries) with
  | None, [] -> []
  | Some _, _ | None, _ :: _ ->
      Ast_helper.with_default_loc (migrate_loc `Gen) (fun () ->
          [ Ast_helper.Str.eval ~attrs
              (Ast_helper.Exp.record
                 (List.filter_opt
                    [ Option.map repl ~f:(fun repl ->
                          ( { Location.txt = Longident.Lident "repl"; loc = Location.none }
                          , None
                          , Some repl ))
                    ; (match libraries with
                      | [] -> None
                      | _ :: _ as l ->
                          Some
                            ( { Location.txt = Longident.Lident "libraries"
                              ; loc = Location.none
                              }
                            , None
                            , Some
                                (Ast_helper.Exp.list
                                   (List.map l ~f:Ast_helper.Exp.string)) ))
                    ])
                 None)
          ])

let attribute_payload ?repl ~(loc : Location.t) expr_opt =
  let report subloc fmt =
    Location.raise_errorf
      ~loc:
        (update_loc subloc (fun pos -> { pos with pos_fname = loc.loc_start.pos_fname }))
      fmt
  in
  match (expr_opt : Parsetree.expression option) with
  | None -> (
      match repl with
      | None -> report loc "missing repl field in attribute"
      | Some repl -> { repl; libraries = [] })
  | Some ({ pexp_desc = Pexp_record (l, None); _ } as expr) ->
      let repl =
        match
          ( repl
          , List.find_map l ~f:(function
              | { txt = Lident "repl"; _ }, None, Some expr -> Some expr
              | _ -> None) )
        with
        | None, None -> report expr.pexp_loc "missing repl field in attribute"
        | Some s, None | None, Some s -> s
        | Some _, Some _ -> report expr.pexp_loc "two repl expressions specified"
      in
      let libraries =
        match
          List.find_map l ~f:(function
            | { txt = Lident "libraries"; _ }, None, Some expr -> Some expr
            | _ -> None)
        with
        | None -> []
        | Some expr -> (
            match expr.pexp_desc with
            | Pexp_list l ->
                List.map l ~f:(function
                  | { pexp_desc =
                        Pexp_constant { pconst_desc = Pconst_string (s, _, _); _ }
                    ; _
                    } ->
                      s
                  | _ ->
                      report expr.pexp_loc
                        "the libraries field should consist of string literals")
            | _ ->
                report expr.pexp_loc
                  "the libraries field should consist of a list literal of string \
                   literals")
      in
      { repl; libraries }
  | Some expr -> report expr.pexp_loc "attribute payload not in expected format"

let internalize_attribute (expr : P.expression) =
  if
    Attr.exists expr.pexp_attributes (Attr.reorder `Source)
    || Attr.exists expr.pexp_attributes (Attr.commutes `Source)
  then
    { expr with
      pexp_attributes =
        List.map expr.pexp_attributes ~f:(fun attr ->
            if attr.attr_name.txt =: Attr.reorder `Source
            then
              { attr with
                attr_name = { attr.attr_name with txt = Attr.reorder `Internal }
              }
            else if attr.attr_name.txt =: Attr.commutes `Source
            then
              { attr with
                attr_name = { attr.attr_name with txt = Attr.commutes `Internal }
              }
            else attr)
    }
  else expr

let internalize_attribute_mapper =
  let super = Ast_mapper.default_mapper in
  { super with expr = (fun self expr -> super.expr self (internalize_attribute expr)) }

let fmexpr_of_uexpr ~fmconf source e =
  let e_str = Format.asprintf "%a" Uast.Pprintast.expression e in
  let expr =
    Ocamlformat_lib.Extended_ast.Parse.ast Expression
      ~ocaml_version:(ocaml_version fmconf) ~preserve_beginend:false
      ~input_name:(migrate_filename source) e_str
  in
  expr
  |> Ocamlformat_lib.Extended_ast.map Expression internalize_attribute_mapper
  |> Ocamlformat_lib.Extended_ast.map Expression drop_concrete_syntax_constructs

let fmexpr_of_fmexpr source e =
  let pos_fname = migrate_filename source in
  let super = Ast_mapper.default_mapper in
  let self =
    { super with
      location = (fun _self loc -> update_loc loc (fun pos -> { pos with pos_fname }))
    ; expr = (fun self expr -> super.expr self (internalize_attribute expr))
    }
  in
  e
  |> Ocamlformat_lib.Extended_ast.map Expression self
  |> Ocamlformat_lib.Extended_ast.map Expression drop_concrete_syntax_constructs

let fmtype_of_typedtree ~fmconf source env typ =
  let printed_typ =
    Printtyp.wrap_printing_env ~error:true env (fun () ->
        Format.asprintf "%a" Printtyp.type_expr typ)
  in
  let lexbuf = Lexing.from_string printed_typ in
  Lexing.set_filename lexbuf (migrate_filename source);
  Parse.core_type ~ocaml_version:(Some (ocaml_version' fmconf)) lexbuf

exception Nope of string

let might_rely_on_type_based_disambiguation expr =
  let super = Ast_mapper.default_mapper in
  let self =
    { super with
      expr =
        (fun self expr ->
          match expr with
          (* The list syntax could be overridden to mean something else depending
             on type information, but it seems it would pessimizes things a lot to
             assume that all list literal are such lists. Similarly, we special case
             special case options and bools *)
          | { pexp_desc = Pexp_construct ({ txt = Lident s; _ }, _); _ } -> (
              match s with
              | "true" | "false" | "None" | "Some" | "()" -> super.expr self expr
              | _ -> Stdlib.raise_notrace Stdlib.Exit)
          | { pexp_desc = Pexp_record (labels, _); _ }
            when List.exists labels ~f:(function
                   | { txt = Lident _; _ }, _, _ -> true
                   | _ -> false) ->
              Stdlib.raise_notrace Stdlib.Exit
          (* The question we're answering is: if the current expression is typed with a
             useful expected type (from the context), does it affect type
             inference. While [record.field] is subject to type based disambiguation
             (specifically the expected type of [record] matters), the expected type for
             [record.field] doesn't matter, so we don't consider that Pexp_field relies
             on type information flow. This is actually somewhat incorrect because when
             the current expression is a function literal [fun record -> record.field],
             then the expected type flows from the literal to the variable record to the
             field.  It might be rather pessismistic to treat all record accesses this
             way though, so maybe it's ok to be under approximating until we take this
             specific case into account.
             This might be also a case where taking cmt information may help: if we
             can lookup unqualified constructors in the environment at that point and
             find the same constructors as the typed ast contains, then type information
             is not necessary. *)
          | { pexp_desc =
                ( Pexp_apply _ | Pexp_letop _ | Pexp_indexop_access _ | Pexp_prefix _
                | Pexp_infix _ )
            ; _
            } ->
              (* don't think the type info propagates from return type to the function or
                 parameters *)
              expr
          | { pexp_desc = Pexp_let (_, expr, _); _ } -> self.expr self expr
          | { pexp_desc =
                ( Pexp_ident _ | Pexp_constant _ | Pexp_function _ | Pexp_match _
                | Pexp_try _ | Pexp_tuple _ | Pexp_variant _ | _ )
            ; _
            } ->
              super.expr self expr)
    ; pat =
        (fun self pat ->
          match pat with
          | { ppat_desc = Ppat_construct ({ txt = Lident s; _ }, _); _ } -> (
              match s with
              | "true" | "false" | "None" | "Some" | "()" -> super.pat self pat
              | _ -> Stdlib.raise_notrace Stdlib.Exit)
          | { ppat_desc = Ppat_record (labels, _); _ }
            when List.exists labels ~f:(function
                   | { txt = Lident _; _ }, _, _ -> true
                   | _ -> false) ->
              Stdlib.raise_notrace Stdlib.Exit
          | _ -> super.pat self pat)
    }
  in
  try
    ignore (self.expr self expr);
    false
  with Stdlib.Exit -> true

let can_be_dead_code_eliminated =
  let super = Ast_mapper.default_mapper in
  let self =
    { super with
      pat =
        (fun self pat ->
          match pat.ppat_desc with
          | Ppat_lazy _ -> Exn.raise_without_backtrace Stdlib.Exit
          | _ -> super.pat self pat)
    ; expr =
        (fun self expr ->
          (* We could probably jump into the definitions of functions to see what
             they do, instead of assuming all functions are side effecting. We could
             also exclude mutations of local state. *)
          match expr.pexp_desc with
          | Pexp_apply _ | Pexp_while _ | Pexp_send _ | Pexp_new _ | Pexp_setfield _
          | Pexp_setinstvar _ | Pexp_assert _ | Pexp_letop _ | Pexp_indexop_access _
          | Pexp_prefix _ | Pexp_infix _ ->
              Exn.raise_without_backtrace Stdlib.Exit
          | _ -> super.expr self expr)
    }
  in
  fun field (v : 'a) ->
    match (field self) self v with exception Stdlib.Exit -> false | (_ : 'a) -> true

type exec =
  | Const
  | Special of [ `Call | `Mutation | `Marker | `Loop ]
  | Sequence of exec * exec
  | Unordered of exec * exec
  | Either of exec * exec

let sequence = function Const, x | x, Const -> x | x, y -> Sequence (x, y)
let unordered = function Const, x | x, Const -> x | x, y -> Unordered (x, y)
let either = function Const, Const -> Const | x, y -> Either (x, y)

let unordereds ts =
  Option.value (List.reduce ts ~f:(fun x y -> Unordered (x, y))) ~default:Const

let rec exec_of_expr (expr : P.expression) =
  match expr.pexp_desc with
  | Pexp_ident id -> (
      match id.txt with Lident "#ocamlmigmarker" -> Special `Marker | _ -> Const)
  | Pexp_constant _ -> Const
  | Pexp_let (bindings, body, _) -> Sequence (exec_of_bindings bindings, exec_of_expr body)
  | Pexp_function (params, _, body) ->
      if
        List.exists params ~f:(fun param ->
            match param.pparam_desc with
            | Pparam_newtype _ -> false
            | Pparam_val _ -> true)
      then Const
      else exec_of_function_body body
  | Pexp_apply (fun_, args) ->
      let args_exec = exec_of_expr_list (fun_ :: List.map args ~f:snd) in
      if Sattr.exists Sattr.commutes expr.pexp_attributes
      then args_exec
      else Sequence (args_exec, Special `Call)
  | Pexp_match (e, cases) -> Sequence (exec_of_expr e, exec_of_cases cases)
  | Pexp_try (e, cases) -> either (exec_of_expr e, exec_of_cases cases)
  | Pexp_tuple l -> unordereds (List.map l ~f:exec_of_expr)
  | Pexp_construct (_, opt) -> exec_of_expr_opt opt
  | Pexp_variant (_, opt) -> exec_of_expr_opt opt
  | Pexp_record (fields, opt) ->
      unordereds
        (exec_of_expr_opt opt
        :: List.map fields ~f:(fun (_, _, opt) -> exec_of_expr_opt opt))
  | Pexp_field (e, _) -> exec_of_expr e
  | Pexp_setfield (e1, _, e2) ->
      sequence (unordered (exec_of_expr e1, exec_of_expr e2), Special `Mutation)
  | Pexp_array l -> exec_of_expr_list l
  | Pexp_list l -> exec_of_expr_list l
  | Pexp_ifthenelse (branches, rest) ->
      List.fold_right branches
        ~init:(exec_of_expr_opt (Option.map rest ~f:fst))
        ~f:(fun branch acc ->
          Sequence (exec_of_expr branch.if_cond, either (exec_of_expr branch.if_body, acc)))
  | Pexp_sequence (e1, e2) -> sequence (exec_of_expr e1, exec_of_expr e2)
  | Pexp_while _ -> Special `Loop
  | Pexp_for (_, e1, e2, _, _) ->
      sequence (unordered (exec_of_expr e1, exec_of_expr e2), Special `Loop)
  | Pexp_constraint (e, _) -> exec_of_expr e
  | Pexp_coerce (e, _, _) -> exec_of_expr e
  | Pexp_send (e, _) -> sequence (exec_of_expr e, Special `Call)
  | Pexp_new _ -> Special `Call
  | Pexp_setinstvar (_, e) -> sequence (exec_of_expr e, Special `Mutation)
  | Pexp_override l -> exec_of_expr_list (List.map l ~f:snd)
  | Pexp_letmodule _ | Pexp_letexception _ -> Special `Call
  | Pexp_assert e -> either (exec_of_expr e, Special `Call)
  | Pexp_lazy _ -> Const
  | Pexp_object _ -> Special `Call
  | Pexp_pack _ -> Special `Call
  | Pexp_open (_, e) -> exec_of_expr e
  | Pexp_letopen _ -> Special `Call
  | Pexp_letop _ -> Special `Call
  | Pexp_extension _ -> Special `Call
  | Pexp_unreachable -> Const
  | Pexp_hole -> Const
  | Pexp_beginend e | Pexp_parens e -> exec_of_expr e
  | Pexp_cons l -> exec_of_expr_list l
  | Pexp_indexop_access _ -> Special `Call
  | Pexp_prefix (_, e) -> sequence (exec_of_expr e, Special `Call)
  | Pexp_infix (_, e1, e2) ->
      sequence (unordered (exec_of_expr e1, exec_of_expr e2), Special `Call)

and exec_of_expr_opt opt = match opt with None -> Const | Some e -> exec_of_expr e
and exec_of_expr_list l = unordereds (List.map l ~f:exec_of_expr)

and exec_of_function_body = function
  | Pfunction_cases _ -> Const
  | Pfunction_body body -> exec_of_expr body

and exec_of_bindings (bindings : P.value_bindings) =
  unordereds
    (List.map bindings.pvbs_bindings ~f:(fun binding ->
         sequence (exec_of_function_body binding.pvb_body, exec_of_pat binding.pvb_pat)))

and exec_of_pat pat =
  if can_be_dead_code_eliminated __.pat pat then Const else Special `Call

and exec_of_cases (cases : P.case list) =
  let maybe_seq a b = sequence (a, either (b, Const)) in
  List.map cases ~f:(fun case ->
      either
        ( (match case.pc_guard with
          | None -> maybe_seq (exec_of_pat case.pc_lhs) (exec_of_expr case.pc_rhs)
          | Some guard ->
              maybe_seq (exec_of_pat case.pc_lhs)
                (maybe_seq (exec_of_expr guard) (exec_of_expr case.pc_rhs)))
        , Const ))
  |> unordereds

let iter_bound_vars field v f =
  (* only for the regular scope, not modules or type vars *)
  let super = Ast_mapper.default_mapper in
  let self =
    { super with
      pat =
        (fun self pat ->
          (match pat with
          | { ppat_desc = Ppat_var p; _ } -> f p.txt
          | { ppat_desc = Ppat_alias (_, p); _ } -> f p.txt
          | _ -> ());
          match pat with
          | { ppat_desc = Ppat_or ps; _ } ->
              (* Assume well typedness, in which case bound names are the same in all
                 branches. *)
              self.pat self (List.hd_exn ps)
          | _ -> super.pat self pat)
    }
  in
  ignore ((field self) self v)

type use =
  | Ambig
  | Once
  | More of int

let add_use = function Ambig -> Ambig | Once -> More 2 | More n -> More (n + 1)

let map_pat_root (super : Ast_mapper.mapper) f =
  let nested_pattern = ref false in
  fun self pat ->
    if !nested_pattern
    then super.pat self pat
    else
      let pat' = f pat in
      nested_pattern := true;
      let pat'' = super.pat self pat' in
      nested_pattern := false;
      pat''

let mapper_with_bound_vars (new_scope, end_scope) =
  let empty_string_set = Set.empty (module String) in
  let free_vars = ref (Map.empty (module String)) in
  let ctx = ref (false, empty_string_set, empty_string_set) in
  let restoring_context f =
    let scope = new_scope () in
    let ctx_before = !ctx in
    let v = f () in
    ctx := ctx_before;
    end_scope scope;
    v
  in
  let super = Ast_mapper.default_mapper in
  let found_open () =
    let _ambig, bound, _bound_since_open = !ctx in
    ctx := (true, bound, empty_string_set)
  in
  let found_var var =
    let ambig, bound_vars, _bound_vars_since_ambig = !ctx in
    if not (Set.mem bound_vars var)
    then
      match Map.find !free_vars var with
      | Some use ->
          free_vars :=
            Map.set !free_vars ~key:var ~data:(if ambig then Ambig else add_use use)
      | None ->
          free_vars :=
            Map.add_exn !free_vars ~key:var ~data:(if ambig then Ambig else Once)
  in
  let self =
    let map_constraint (self : Ast_mapper.mapper) (c : P.type_constraint) :
        P.type_constraint =
      match c with
      | Pconstraint ty -> Pconstraint (self.typ self ty)
      | Pcoerce (ty1, ty2) ->
          Pcoerce (Option.map ty1 ~f:(self.typ self), self.typ self ty2)
    in
    let map_function_body (self : Ast_mapper.mapper) (body : P.function_body) :
        P.function_body =
      match body with
      | Pfunction_body e -> Pfunction_body (self.expr self e)
      | Pfunction_cases (cases, loc, attributes) ->
          let cases = self.cases self cases in
          let loc = self.location self loc in
          let attributes = self.attributes self attributes in
          Pfunction_cases (cases, loc, attributes)
    in
    { super with
      pat =
        map_pat_root super (fun pat ->
            iter_bound_vars __.pat pat (fun v ->
                let ambig, bound, bound_since_open = !ctx in
                ctx := (ambig, Set.add bound v, Set.add bound_since_open v));

            pat)
    ; case =
        (fun self { pc_lhs; pc_guard; pc_rhs } ->
          (* Here and in many other cases, it seems like we could just call
             [restoring_context (fun () -> super.case self case)], but that doesn't
             work because ast_mapper doesn't traverse in evaluation order, it simply
             builds { field = map field1; field2 = map field2 } sort of expressions,
             which in practice are evaluated right-to-left, so patterns traversed after
             the expression they bind names in. *)
          restoring_context (fun () ->
              let pc_lhs' = self.pat self pc_lhs in
              let pc_guard' = Option.map ~f:(self.expr self) pc_guard in
              let pc_rhs' = self.expr self pc_rhs in
              ({ pc_lhs = pc_lhs'; pc_guard = pc_guard'; pc_rhs = pc_rhs' } : P.case)))
    ; expr =
        (fun self expr ->
          (* maybe we could use the cmt information to do better? *)
          match expr.pexp_desc with
          | Pexp_ident { txt = Lident var; _ } ->
              found_var var;
              expr
          | Pexp_let (bindings, body, z1) ->
              restoring_context (fun () ->
                  let bindings' = self.value_bindings self bindings in
                  let body' = self.expr self body in
                  { expr with pexp_desc = Pexp_let (bindings', body', z1) })
          | Pexp_open (id, body) ->
              let body' =
                restoring_context (fun () ->
                    found_open ();
                    self.expr self body)
              in
              { expr with pexp_desc = Pexp_open (id, body') }
          | Pexp_letopen (open_infos, body) ->
              let open_infos' =
                { open_infos with
                  popen_expr = self.module_expr self open_infos.popen_expr
                }
              in
              let body' =
                restoring_context (fun () ->
                    found_open ();
                    self.expr self body)
              in
              { expr with pexp_desc = Pexp_letopen (open_infos', body') }
          | Pexp_function (params, typ_opt, body) ->
              restoring_context (fun () ->
                  let params' =
                    List.map params ~f:(fun param ->
                        match param.pparam_desc with
                        | Pparam_newtype _ -> param
                        | Pparam_val (label, edefault, pat) ->
                            let edefault' = Option.map edefault ~f:(self.expr self) in
                            let label' = label in
                            let pat' = self.pat self pat in
                            { param with
                              pparam_desc = Parsetree.Pparam_val (label', edefault', pat')
                            })
                  in
                  let typ_opt' = Option.map typ_opt ~f:(map_constraint self) in
                  let body' = map_function_body self body in
                  { expr with pexp_desc = Pexp_function (params', typ_opt', body') })
          | Pexp_match (e, cases) ->
              let e' = self.expr self e in
              let cases' = self.cases self cases in
              { expr with pexp_desc = Pexp_match (e', cases') }
          | Pexp_try (e, cases) ->
              let e' = self.expr self e in
              let cases' = self.cases self cases in
              { expr with pexp_desc = Pexp_try (e', cases') }
          | Pexp_for (p, e1, e2, dir, body) ->
              let e1' = self.expr self e1 in
              let e2' = self.expr self e2 in
              let p', body' =
                restoring_context (fun () ->
                    let p' = self.pat self p in
                    (p', self.expr self body))
              in
              { expr with pexp_desc = Pexp_for (p', e1', e2', dir, body') }
          | Pexp_letop letop ->
              let bop_exp (bop : P.binding_op) =
                let args', expr' =
                  restoring_context (fun () ->
                      let args' =
                        List.map bop.pbop_args ~f:(fun param ->
                            match param with
                            | { pparam_desc = Pparam_newtype _; _ } -> param
                            | { pparam_desc = Pparam_val (label, edefault, pat); _ } ->
                                let edefault' = Option.map edefault ~f:(self.expr self) in
                                let pat' = self.pat self pat in
                                { param with
                                  pparam_desc = Pparam_val (label, edefault', pat')
                                })
                      in
                      (args', self.expr self bop.pbop_exp))
                in
                { bop with pbop_args = args'; pbop_exp = expr' }
              in
              let bop_pat (bop : P.binding_op) =
                { bop with pbop_pat = self.pat self bop.pbop_pat }
              in
              let let_ = bop_exp letop.let_ and ands = List.map ~f:bop_exp letop.ands in
              restoring_context (fun () ->
                  let let_ = bop_pat let_ and ands = List.map ~f:bop_pat ands in
                  let letop =
                    { letop with let_; ands; body = self.expr self letop.body }
                  in
                  { expr with pexp_desc = Pexp_letop letop })
          | _ -> super.expr self expr)
    ; value_bindings =
        (fun self bindings ->
          let vb_expr (vb : P.value_binding) =
            let args', body' =
              restoring_context (fun () ->
                  let args' =
                    List.map vb.pvb_args ~f:(fun param ->
                        match param with
                        | { pparam_desc = Pparam_newtype _; _ } -> param
                        | { pparam_desc = Pparam_val (label, edefault, pat); _ } ->
                            let edefault' = Option.map edefault ~f:(self.expr self) in
                            let pat' = self.pat self pat in
                            { param with
                              pparam_desc = Pparam_val (label, edefault', pat')
                            })
                  in
                  (args', map_function_body self vb.pvb_body))
            in
            { vb with pvb_args = args'; pvb_body = body' }
          in
          let vb_pat (vb : P.value_binding) =
            let pat' = self.pat self vb.pvb_pat in
            { vb with pvb_pat = pat' }
          in
          let vb_other (vb : P.value_binding) =
            { vb with pvb_attributes = self.ext_attrs self vb.pvb_attributes }
          in
          match bindings.pvbs_rec with
          | Nonrecursive ->
              { bindings with
                pvbs_bindings =
                  bindings.pvbs_bindings
                  |> List.map ~f:vb_expr
                  |> List.map ~f:vb_pat
                  |> List.map ~f:vb_other
              }
          | Recursive ->
              { bindings with
                pvbs_bindings =
                  bindings.pvbs_bindings
                  |> List.map ~f:vb_pat
                  |> List.map ~f:vb_expr
                  |> List.map ~f:vb_other
              })
    }
  in
  (free_vars, ctx, self)

let free_vars expr =
  let free_vars, _ctx, mapper = mapper_with_bound_vars (Fn.id, Fn.id) in
  ignore (mapper.expr mapper expr);
  !free_vars

let var_names method_ v =
  let free_vars, ctx, super = mapper_with_bound_vars (Fn.id, Fn.id) in
  let ambiguous = ref (Set.empty (module String)) in
  let all = ref (Set.empty (module String)) in
  let found_var var =
    let ambig, _, bound_since_open = !ctx in
    if ambig && not (Set.mem bound_since_open var)
    then ambiguous := Set.add !ambiguous var
  in
  let self =
    { super with
      pat =
        map_pat_root super (fun pat ->
            iter_bound_vars __.pat pat (fun var -> all := Set.add !all var);
            pat)
    ; expr =
        (fun self expr ->
          (match expr.pexp_desc with
          | Pexp_ident { txt = Lident var; _ } -> found_var var
          | _ -> ());
          super.expr self expr)
    }
  in
  ignore ((method_ self) self v);
  (!free_vars, !all, !ambiguous)

let alpha_convert_pattern subst pat =
  if Map.is_empty subst
  then pat
  else
    let super = Ast_mapper.default_mapper in
    let self =
      { super with
        pat =
          (fun self pat ->
            let pat = super.pat self pat in
            match pat with
            | { ppat_desc = Ppat_var p; _ } -> (
                match Map.find subst p.txt with
                | None -> pat
                | Some nvar -> { pat with ppat_desc = Ppat_var { p with txt = nvar } })
            | { ppat_desc = Ppat_alias (p1, p2); _ } -> (
                match Map.find subst p2.txt with
                | None -> pat
                | Some nvar ->
                    { pat with ppat_desc = Ppat_alias (p1, { p2 with txt = nvar }) })
            | _ -> pat)
      }
    in
    self.pat self pat

let substitute subst e =
  if Map.is_empty subst
  then e
  else
    let fvs = free_vars (Ast_helper.Exp.tuple (Map.data subst)) in
    (* The caller should ensure this isn't called in ambiguous cases (caused by open),
         we should avoid captures (both by taking scopes into account here, and by
         ensuring that [e] has no free vars that are bound anywhere in [by]).

       There are stil many flaws:
       - captures in other scopes (modules, module types, constructors, types)
       - type variables, since they are scoped at the level of structure items
       - we assume that side effects commute, which has no reason to be true
    *)
    let _, all_bound_names, _ = var_names __.expr e in
    let fvs_subst = ref (fvs, Map.map subst ~f:(fun e -> `Expr e)) in
    let rec new_name ?(count = 2) orig =
      if count >= 100 then failwith "can't find a new name";
      let proposed = orig ^ Int.to_string count in
      if Map.mem (fst !fvs_subst) proposed || Set.mem all_bound_names proposed
      then new_name ~count:(count + 1) orig
      else proposed
    in
    let _free_vars, ctx, super =
      mapper_with_bound_vars ((fun () -> !fvs_subst), fun old -> fvs_subst := old)
    in
    let find_repl var =
      let subst = snd !fvs_subst in
      match Map.find subst var with
      | None -> None
      | Some _ as opt ->
          let ambig, bound_vars, _ = !ctx in
          if (not ambig) && not (Set.mem bound_vars var) then opt else None
    in
    let self =
      { super with
        pat =
          map_pat_root super (fun pat ->
              let bound_vars =
                set_from_iter
                  (module String)
                  (fun yield -> iter_bound_vars __.pat pat yield)
              in
              (* ideally, we'd only alpha convert if there are occurrences of the ident
                 we care about below. We could do this by keeping a count in the
                 context of real substitution (as opposed to alpha conversion). If the
                 count is 0 in a subtree, we revert the alpha renaming. Maybe it needs
                 to be a map for each conversion. In that case, maybe we should have
                 subst + alpha conv as separate maps, not sure. *)
              let to_alpha_convert =
                list_from_iter (fun yield ->
                    Set.iter bound_vars ~f:(fun bound_var ->
                        if Map.mem fvs bound_var then yield (bound_var, new_name bound_var)))
              in
              let pat' =
                alpha_convert_pattern
                  (Map.of_alist_exn (module String) to_alpha_convert)
                  pat
              in
              List.iter to_alpha_convert ~f:(fun (before, after) ->
                  fvs_subst :=
                    ( fst !fvs_subst
                    , Map.set (snd !fvs_subst) ~key:before ~data:(`Lident after) ));
              pat')
      ; expr =
          (fun self expr ->
            let expr = super.expr self expr in
            match expr.pexp_desc with
            | Pexp_ident { txt = Lident var; loc } -> (
                match find_repl var with
                | None -> expr
                | Some (`Lident by) ->
                    { expr with pexp_desc = Pexp_ident { txt = Lident by; loc } }
                | Some (`Expr by) -> by)
            | _ -> expr)
      }
    in
    self.expr self e

let duplicatable (e : P.expression) =
  (* Here, the question is not just whether duplicating the expression would cause
     duplicate side effects or similar, but also whether duplicating a bit of source is
     fine from a readability/maintenance perspective. It's not clear that duplicating
     an integer is ok, although if a migration involves duplicating an integer, maybe
     it means that the two numbers don't have to be in sync, and so perhaps the
     duplication would be fine. *)
  match e with
  | { pexp_desc = Pexp_ident _ | Pexp_construct (_, None) | Pexp_variant (_, None); _ } ->
      true
  | _ -> false

let rec execution_commutes (e : P.expression) =
  (* The question is whether the expression can be moved anywhere, even inside a
     function or a loop, past side effects, into the true branch of an if, etc.

     So no changes to mutable state are allowed, but no reads from mutable are allowed
     either.

     In principle, we could duplicate some amount of work (like closure allocation), but
     we should ensure it's O(1). *)
  match e.pexp_desc with
  | Pexp_ident _ -> true
  | Pexp_constant _ -> true
  | Pexp_construct (_, opt) | Pexp_variant (_, opt) -> execution_commutes_opt opt
  | Pexp_tuple l -> execution_commutes_list l
  | Pexp_function (params, _, body) -> (
      List.exists params ~f:(fun param ->
          match param.pparam_desc with Pparam_newtype _ -> false | Pparam_val _ -> true)
      ||
      match body with
      | Pfunction_cases _ -> true
      | Pfunction_body body -> execution_commutes body)
  | Pexp_parens e -> execution_commutes e
  | _ -> false

and execution_commutes_opt opt =
  match opt with None -> true | Some e -> execution_commutes e

and execution_commutes_list l = List.for_all l ~f:execution_commutes

let check_marker_executed_before_all_the_things exec =
  let rec loop exec =
    match exec with
    | Sequence (e1, e2) -> (
        match loop e1 with
        | `Marker res -> `Marker res
        | `Effect -> `Effect
        | `Const -> loop e2)
    | Either (e1, e2) -> (
        match (loop e1, loop e2) with
        | `Marker _, `Marker _ -> assert false
        | `Const, `Const -> `Const
        | `Effect, `Effect -> `Effect
        | (`Effect | `Const), `Marker _ | `Marker _, (`Effect | `Const) -> `Marker false
        | `Const, `Effect | `Effect, `Const -> `Effect)
    | Unordered (e1, e2) -> (
        match (loop e1, loop e2) with
        | `Marker _, `Marker _ -> assert false
        | `Const, e | e, `Const -> e
        | `Effect, `Marker _ | `Marker _, `Effect -> `Marker false
        | `Effect, `Effect -> `Effect)
    | Special `Loop -> `Effect
    | Special `Call -> `Effect
    | Special `Mutation -> `Effect
    | Special `Marker -> `Marker true
    | Const -> `Const
  in
  loop exec

let substitution_would_preserve_execution var expr =
  (* This assumes there is a single occurrence of the variable to be substituted. *)
  let _, ctx, super = mapper_with_bound_vars (Fn.id, Fn.id) in
  let marker = Ast_helper.Exp.ident' (Lident "#ocamlmigmarker") in
  let find_repl var' =
    if var =: var'
    then
      let ambig, bound_vars, _ = !ctx in
      if (not ambig) && not (Set.mem bound_vars var) then Some marker else None
    else None
  in
  let self =
    { super with
      expr =
        (fun self expr ->
          let expr = super.expr self expr in
          match expr.pexp_desc with
          | Pexp_ident { txt = Lident var; _ } -> (
              match find_repl var with None -> expr | Some by -> by)
          | _ -> expr)
    }
  in
  let marked_expr = self.expr self expr in
  match check_marker_executed_before_all_the_things (exec_of_expr marked_expr) with
  | `Marker b -> b
  | `Const | `Effect ->
      (* Haven't seen the marker, which means it's inside a loop, function body, lazy or
         after a side effect. *)
      false

let commute (arg1 : Asttypes.arg_label) (arg2 : Asttypes.arg_label) =
  match (arg1, arg2) with
  | Nolabel, Nolabel -> false
  | Nolabel, _ | _, Nolabel -> true
  | (Labelled s1 | Optional s1), (Labelled s2 | Optional s2) -> s1.txt <>: s2.txt

let loc_if_from_original_file =
  let exception E of Lexing.position in
  let super = Ast_mapper.default_mapper in
  let self =
    { super with
      expr =
        (fun self expr ->
          let pos = expr.pexp_loc.loc_start in
          if pos.pos_fname <>: migrate_filename_import && not expr.pexp_loc.loc_ghost
          then Exn.raise_without_backtrace (E pos);
          super.expr self expr)
    }
  in
  fun expr ->
    (* A parameter from user code doesn't have to be on the surface, for instance
       if we transform ~a into ~a:(Some a), the Some constructor is created by
       the migration. *)
    match self.expr self expr with
    | exception E pos -> `Source pos.pos_cnum
    | (_ : P.expression) -> `Migration

let commute_list commute args =
  if
    List.length args > 200
    (* this code is quadratic, so leave pathological cases alone *)
  then args
  else
    let l =
      List.map args ~f:(fun ((_, e) as arg) -> (loc_if_from_original_file e, arg))
    in
    (* In principle, if everything commutes and all arguments are source code, this
       should degenerate to a bubblesort. *)
    let rec push_down_one elt = function
      | [] -> None
      | elt' :: rest ->
          if commute (fst (snd elt)) (fst (snd elt'))
          then
            let is_useful_to_push_down =
              match (elt, elt') with
              | (`Source i, _), (`Source i', _) -> i > i'
              | ((`Source _ | `Migration), _), ((`Source _ | `Migration), _) -> false
            in
            match push_down_one elt rest with
            | Some l -> Some (elt' :: l)
            | None -> if is_useful_to_push_down then Some (elt' :: elt :: rest) else None
          else None
    in
    let rec push_down = function
      | x :: xs -> (
          let xs = push_down xs in
          match push_down_one x xs with None -> x :: xs | Some l -> l)
      | [] -> []
    in
    push_down l |> List.map ~f:snd

let commute_args (args : function_arg list) = commute_list commute args

let expr_is_var (e : P.expression) var =
  match e with
  | { pexp_desc = Pexp_ident { txt = Lident var'; _ }; _ } -> var =: var'
  | _ -> false

let push_down_arg_as_much_as_possible var (args : function_arg list) =
  let rec relocate arg = function
    | [] -> [ arg ]
    | arg' :: args ->
        if commute (fst arg) (fst arg')
        then arg' :: relocate arg args
        else arg :: arg' :: args
  in
  let rec locate rev_prefix = function
    | [] -> None
    | ((_, e) as arg) :: args ->
        if expr_is_var e var
        then Some (List.rev_append rev_prefix (relocate arg args))
        else locate (arg :: rev_prefix) args
  in
  locate [] args

let try_partial_eta_reduce ~ctx (body : P.expression)
    (remaining_param_vars : _ Location.loc list) =
  let fvs = free_vars body in
  if
    not
      (List.for_all remaining_param_vars ~f:(fun var ->
           match Map.find fvs var.txt with Some Once -> true | _ -> false))
  then Error "free_vars"
  else
    match body with
    | { pexp_desc = Pexp_apply (fun_, args); _ } -> (
        match
          List.fold_left remaining_param_vars ~init:(Some args) ~f:(fun args var ->
              match args with
              | None -> None
              | Some args -> push_down_arg_as_much_as_possible var.txt args)
        with
        | None -> Error "Missing_arg"
        | Some args' -> (
            assert (List.length args = List.length args');
            let leading_args, trailing_args =
              List.split_n args' (List.length args - List.length remaining_param_vars)
            in
            if
              List.for_all2_exn trailing_args remaining_param_vars ~f:(fun arg var ->
                  expr_is_var (snd arg) var.txt)
            then Ok { body with pexp_desc = Pexp_apply (fun_, leading_args) }
            else
              match remaining_param_vars with
              | [ var ] when ctx.has_ppx_partial ->
                  Ok
                    { body with
                      pexp_desc =
                        Pexp_apply
                          ( fun_
                          , List.map args ~f:(fun arg ->
                                if expr_is_var (snd arg) var.txt
                                then (fst arg, Ast_helper.Exp.ident' (Lident "__"))
                                else arg) )
                    }
              | _ -> Error "Not_trailing"))
    | _ -> Error "Not_apply"

let line_up_params_and_args ~ctx ~apply_loc params (args : function_arg list) body =
  try
    let string_from_arg_label : Asttypes.arg_label -> _ = function
      | Asttypes.Nolabel -> ""
      | Labelled s | Optional s -> s.txt
    in
    let alabel (arg_label, _) = string_from_arg_label arg_label in
    let plabel (p : P.expr_function_param) =
      match p.pparam_desc with
      | Pparam_newtype _ -> raise (Nope "newtype")
      | Pparam_val (label, _, _) -> string_from_arg_label label
    in
    let params =
      (* reverse the comparison so the anon args are last *)
      List.stable_sort
        ~compare:
          (Comparable.reverse (fun p1 p2 -> String.compare (plabel p1) (plabel p2)))
        params
    in
    let args =
      args
      |> List.stable_sort
           ~compare:
             (Comparable.reverse (fun arg1 arg2 ->
                  String.compare (alabel arg1) (alabel arg2)))
    in
    let rec merge acc (params : P.expr_function_param list) args =
      match (params, args) with
      | [], _ ->
          ( List.rev acc
          , match args with
            | [] -> body
            | _ :: _ -> Ast_helper.Exp.apply ~loc:apply_loc body args )
      | _ :: _, [] -> (
          let all_vars = ref true in
          let remaining_param_vars =
            List.filter_map params ~f:(function
              | { pparam_desc = Pparam_val (Nolabel, _, { ppat_desc = Ppat_var x; _ })
                ; _
                } ->
                  Some x
              | _ ->
                  all_vars := false;
                  None)
          in
          if not !all_vars then raise (Nope "remaining params");
          match try_partial_eta_reduce ~ctx body remaining_param_vars with
          | Error s -> raise (Nope s)
          | Ok body -> (List.rev acc, body))
      | param :: params2, arg :: args2 ->
          let c = Comparable.reverse String.compare (plabel param) (alabel arg) in
          if c = 0
          then merge ((param, Some arg) :: acc) params2 args2
          else if c < 0
          then
            match param.pparam_desc with
            | Pparam_val (Optional _, _, _) -> merge ((param, None) :: acc) params2 args
            | _ -> raise (Nope "missing arg1")
          else raise (Nope ("missing arg2 p:" ^ plabel param ^ " a:" ^ alabel arg))
    in
    Some (merge [] params args)
  with Nope s ->
    if !log then Printf.eprintf "failed to rewrite: %s\n" s;
    None

let can_simplify (loc1 : Location.t) (loc2 : Location.t) =
  (* If the source code or the remplacement contains simplifiable things like [if
       false then ..], we want to leave them alone. But if there is code that is either
       generated by us, or a case where the combination of the source code and the
       replacement interacts, then we want to simplify. *)
  loc1.loc_start.pos_fname =: migrate_filename_gen
  || loc1.loc_start.pos_fname <>: loc2.loc_start.pos_fname

let rec match_pat_and_expr (pat : P.pattern) (expr : P.expression) bindings =
  match (pat, expr) with
  | { ppat_desc = Ppat_any; _ }, _ -> Some (Ok bindings)
  | { ppat_desc = Ppat_var var; _ }, _ ->
      Some (Ok (Map.add_exn bindings ~key:var.txt ~data:(var.loc, expr)))
  | ( { ppat_desc = Ppat_construct (c, arg); _ }
    , { pexp_desc = Pexp_construct (c2, arg2); _ } ) ->
      (* We assume well typedness, and also that we're matching on regular
         constructors, not exceptions/open types. *)
      if Longident.last c.txt =: Longident.last c2.txt
      then
        match (arg, arg2) with
        | None, None -> Some (Ok bindings)
        | Some (_, arg), Some arg2 -> match_pat_and_expr arg arg2 bindings
        | _ -> Some (Error ())
      else None
  | { ppat_desc = Ppat_variant (c, arg); _ }, { pexp_desc = Pexp_variant (c2, arg2); _ }
    ->
      if c.txt.txt =: c2.txt.txt
      then
        match (arg, arg2) with
        | None, None -> Some (Ok bindings)
        | Some arg, Some arg2 -> match_pat_and_expr arg arg2 bindings
        | _ -> Some (Error ())
      else None
  | { ppat_desc = Ppat_or subpats; _ }, _ ->
      List.find_map subpats ~f:(match_pat_and_expr __ expr bindings)
  | _, _ -> Some (Error ())

let match_case_and_expr (case : P.case) (expr : P.expression) =
  match match_pat_and_expr case.pc_lhs expr (Map.empty (module String)) with
  | (None | Some (Error ())) as done_ -> done_
  | Some (Ok bindings) ->
      if Option.is_none case.pc_guard
      then Some (Ok (bindings, case.pc_rhs))
      else Some (Error ())

let match_boolean (expr : P.expression) =
  match expr with
  | { pexp_desc = Pexp_construct (c, None); _ } -> (
      match Longident.last c.txt with
      | "true" -> Ok true
      | "false" -> Ok false
      | _ -> Error ())
  | _ -> Error ()

let has_exception_pattern =
  let super = Ast_mapper.default_mapper in
  let self =
    { super with
      pat =
        (fun self pat ->
          match pat.ppat_desc with
          | Ppat_exception _ -> Stdlib.raise_notrace Stdlib.Exit
          | _ -> super.pat self pat)
    }
  in
  fun meth v ->
    try
      ignore ((meth self) self v);
      false
    with Stdlib.Exit -> true

let execute_match (expr : P.expression) (loc, (cases : P.case list)) ~can_simplify =
  if
    can_simplify expr.pexp_loc loc
    && List.for_all cases ~f:(fun case -> not (has_exception_pattern __.pat case.pc_lhs))
  then
    let last_i = ref 0 in
    match
      List.find_mapi cases ~f:(fun i case ->
          last_i := i;
          match_case_and_expr case expr)
    with
    | None | Some (Error ()) -> None
    | Some (Ok (bindings, rhs)) ->
        (* we need to check whether to let bind or actually substitute, like we do in
           reduce *)
        let subst = Map.map bindings ~f:snd in
        Some (rhs, substitute subst rhs, !last_i)
  else None

let execute_if ~can_simplify ~loc (branches : P.if_branch list) else_ =
  let last_i = ref 0 in
  match
    List.find_mapi branches ~f:(fun i branch ->
        last_i := i;
        if can_simplify loc branch.if_cond.pexp_loc
        then
          match match_boolean branch.if_cond with
          | Ok false -> None
          | Ok true -> Some (Ok branch.if_body)
          | Error () -> Some (Error ())
        else Some (Error ()))
  with
  | None ->
      let e =
        match else_ with
        | None -> Ast_helper.Exp.unit ~loc:(migrate_loc `Gen) ()
        | Some (e, _) -> e
      in
      Some (e, e, !last_i + 1)
  | Some (Error ()) -> None
  | Some (Ok e) -> Some (e, e, !last_i)

let execute_let ~ctx ~type_index ~can_simplify ~value_constraint
    (value_bindings : P.value_bindings) =
  match value_bindings.pvbs_bindings with
  | [ ({ pvb_body = Pfunction_body expr; _ } as vb) ]
    when Option.is_none vb.pvb_constraint
         && can_simplify vb.pvb_pat.ppat_loc expr.pexp_loc
         && can_be_dead_code_eliminated __.value_binding vb -> (
      match match_pat_and_expr vb.pvb_pat expr (Map.empty (module String)) with
      | Some (Ok bindings') when Map.is_empty bindings' ->
          (* optimize let () = () away. We could probably do more. *)
          Some { value_bindings with pvbs_bindings = [] }
      | _ -> None)
  | [ ({ pvb_body =
           Pfunction_body ({ pexp_desc = Pexp_construct (_, Some expr); _ } as body)
       ; pvb_pat = { ppat_desc = Ppat_any; _ }
       ; _
       } as vb)
    ]
    when Option.is_none vb.pvb_constraint && can_simplify body.pexp_loc expr.pexp_loc ->
      Some
        { value_bindings with
          pvbs_bindings =
            [ { vb with
                pvb_body = Pfunction_body expr
              ; pvb_constraint = value_constraint ~ctx:!ctx ~type_index body
              }
            ]
        }
  | _ -> None

let rec map_tails (e : P.expression) f =
  match e with
  | { pexp_desc = Pexp_match (matched, cases); _ } ->
      { e with
        pexp_desc =
          Pexp_match
            ( matched
            , List.map cases ~f:(fun case ->
                  { case with pc_rhs = map_tails case.pc_rhs f }) )
      }
  | { pexp_desc = Pexp_ifthenelse (branches, else_); _ } ->
      { e with
        pexp_desc =
          Pexp_ifthenelse
            ( List.map branches ~f:(fun branch ->
                  { branch with if_body = map_tails branch.if_body f })
            , Option.map else_ ~f:(fun (e, loc) -> (map_tails e f, loc)) )
      }
  | { pexp_desc = Pexp_let (a, e, b); _ } ->
      { e with pexp_desc = Pexp_let (a, map_tails e f, b) }
  | { pexp_desc = Pexp_sequence (a, e); _ } ->
      { e with pexp_desc = Pexp_sequence (a, map_tails e f) }
  | { pexp_desc = Pexp_letmodule (a, b, c, e); _ } ->
      { e with pexp_desc = Pexp_letmodule (a, b, c, map_tails e f) }
  | { pexp_desc = Pexp_letexception (a, e); _ } ->
      { e with pexp_desc = Pexp_letexception (a, map_tails e f) }
  | { pexp_desc = Pexp_open (a, e); _ } ->
      { e with pexp_desc = Pexp_open (a, map_tails e f) }
  | { pexp_desc = Pexp_letopen (a, e); _ } ->
      { e with pexp_desc = Pexp_letopen (a, map_tails e f) }
  | { pexp_desc = Pexp_beginend e; _ } ->
      { e with pexp_desc = Pexp_beginend (map_tails e f) }
  | { pexp_desc = Pexp_parens e; _ } -> { e with pexp_desc = Pexp_parens (map_tails e f) }
  | _ -> f e

let case_of_case e f =
  (* We could simplify more cases than we do. In cases like:
     match
       if ..
       then if .. then Some 1 else Some 2
       else None
     with
     | None -> e1
     | Some x -> e2 x

     Even though we can't just push the match all the way down, we can push it
     partially down, to result in:
       if ..
       then match if .. then 1 else 2 with
            | x -> e2 x
       else e1

     (which can then be simplified further).

     Similarly, we could optimize cases involving try-with while avoiding pushing
     code under the try-with.
  *)
  let unduplicatable_cases_used = ref (Set.empty (module Int)) in
  match
    map_tails e (fun tail ->
        match f tail with
        | None -> Stdlib.raise_notrace Stdlib.Exit
        | Some (orig_rhs, e, i) ->
            let rec rhs_is_duplicatable ~depth (e : P.expression) =
              match e.pexp_desc with
              | Pexp_ident _ | Pexp_construct (_, None) | Pexp_variant (_, None) -> true
              | (Pexp_construct (_, Some e) | Pexp_variant (_, Some e)) when depth >= 1 ->
                  rhs_is_duplicatable ~depth:(depth - 1) e
              | _ -> false
            in
            if not (rhs_is_duplicatable ~depth:1 orig_rhs)
            then (
              if Set.mem !unduplicatable_cases_used i
              then Stdlib.raise_notrace Stdlib.Exit (* avoid code duplication *);
              unduplicatable_cases_used := Set.add !unduplicatable_cases_used i);
            e)
  with
  | exception Stdlib.Exit -> None
  | new_expr -> Some new_expr

let execute_apply ~process_call ~loc (fun_ : P.expression) args =
  if can_simplify fun_.pexp_loc loc then process_call ~apply_loc:loc args fun_ else None

let has_context_match : P.expression -> _ = function
  | { pexp_desc = Pexp_function ([], None, Pfunction_cases (case :: _, _, _)); _ } -> (
      match case.pc_lhs with
      | { ppat_desc = Ppat_extension ({ txt = "context"; _ }, _); _ } -> true
      | _ -> false)
  | _ -> false

let execute_context_match ~type_index (cases : P.case list) ~(orig : P.expression) =
  List.find_map cases ~f:(fun case ->
      match case.pc_lhs with
      | { ppat_desc = Ppat_extension ({ txt = "context"; _ }, PTyp user_typ); _ } -> (
          let ttyp = Uast.type_type (utype_of_fmtype user_typ) in
          match Build.Type_index.exp type_index (Conv.location' orig.pexp_loc) with
          | [ texpr ] ->
              (* maybe root env makes more sense *)
              let does_match =
                Uast.match_typ ~env:texpr.exp_env texpr.exp_type ~user_type:ttyp
              in
              if !log
              then
                print_s
                  [%sexp
                    "context"
                  , (Format.asprintf "%a" Printtyp.type_expr texpr.exp_type : string)
                  , "vs"
                  , (Format.asprintf "%a" Printtyp.type_expr ttyp : string)
                  , ~~(does_match : bool)];
              if does_match then Some case.pc_rhs else None
          | _ -> None)
      | { ppat_desc = Ppat_any; _ } -> Some case.pc_rhs
      | _ -> None)

let simplify_args (args : Fmast.function_arg list) =
  List.filter_map args
    ~f:(fun ((label, arg) as function_arg) : Fmast.function_arg option ->
      match label with
      | Optional label when can_simplify label.loc arg.pexp_loc -> (
          match arg.pexp_desc with
          | Pexp_construct ({ txt = Lident "None"; _ }, None) -> None
          | Pexp_construct ({ txt = Lident "Some"; _ }, Some e) -> Some (Labelled label, e)
          | _ -> Some function_arg)
      | _ -> Some function_arg)

let value_constraint ~ctx ~type_index e : P.value_constraint option =
  if not (might_rely_on_type_based_disambiguation e)
  then None
  else
    match Build.Type_index.exp type_index (Conv.location' e.pexp_loc) with
    | [ texpr ] ->
        Some
          (Pvc_constraint
             { locally_abstract_univars = []
             ; typ =
                 fmtype_of_typedtree ~fmconf:ctx.fmconf `Gen texpr.exp_env texpr.exp_type
             })
    | _ :: _ :: _ -> None
    | [] ->
        let loc = e.pexp_loc in
        Format.printf "@[<2>failed to find@\n%a@]@." Sexplib.Sexp.pp_hum
          [%sexp (Conv.location' loc : Uast.Location.t)];
        None

let simplify ~type_index ~ctx process_call =
  let ctx = ref ctx in
  let super = Ast_mapper.default_mapper in
  let self =
    { super with
      structure_item =
        (fun self si ->
          let no_ppx_partial =
            match si.pstr_desc with
            | Pstr_value { pvbs_bindings = [ vb ]; pvbs_rec = Nonrecursive } ->
                List.exists vb.pvb_attributes.attrs_after ~f:(fun attr ->
                    attr.attr_name.txt =: "migrate.no_ppx_partial")
            | _ -> false
          in
          if no_ppx_partial
          then
            Ref.set_temporarily ctx { !ctx with has_ppx_partial = false } ~f:(fun () ->
                super.structure_item self si)
          else super.structure_item self si)
    ; expr =
        with_log (fun self expr ->
            let expr =
              match expr with
              | { pexp_desc = Pexp_function ([], None, Pfunction_cases (cases, _, _)); _ }
                -> (
                  match Sattr.find Sattr.orig expr.pexp_attributes with
                  | None -> super.expr self expr
                  | Some orig -> (
                      match execute_context_match ~type_index cases ~orig with
                      | None -> self.expr self orig
                      | Some repl -> self.expr self repl))
              | { pexp_desc = Pexp_ifthenelse (branches, else_); _ } -> (
                  match execute_if ~loc:expr.pexp_loc branches else_ ~can_simplify with
                  | None -> super.expr self expr
                  | Some (_, e, _) -> self.expr self e)
              | { pexp_desc = Pexp_match (matched, cases); _ } -> (
                  match execute_match matched (expr.pexp_loc, cases) ~can_simplify with
                  | None -> super.expr self expr
                  | Some (_, new_expr, _) -> self.expr self new_expr)
              | { pexp_desc = Pexp_let (value_bindings, body, loc); _ } -> (
                  match
                    execute_let ~ctx ~type_index value_bindings ~can_simplify
                      ~value_constraint
                  with
                  | None -> super.expr self expr
                  | Some value_bindings -> (
                      match value_bindings.pvbs_bindings with
                      | [] -> self.expr self body
                      | _ :: _ ->
                          self.expr self
                            { expr with pexp_desc = Pexp_let (value_bindings, body, loc) }
                      ))
              | { pexp_desc = Pexp_apply (fun_, args); _ } -> (
                  let args = simplify_args args in
                  let expr = { expr with pexp_desc = Pexp_apply (fun_, args) } in
                  match
                    execute_apply ~process_call:(process_call ~ctx:!ctx)
                      ~loc:expr.pexp_loc fun_ args
                  with
                  | None -> super.expr self expr
                  | Some v -> self.expr self v)
              | _ -> super.expr self expr
            in
            (* should think about how to reach a fixpoint here *)
            match expr with
            | { pexp_desc =
                  Pexp_ifthenelse ([ { if_cond; if_body; if_attrs; if_loc_then } ], else_)
              ; _
              } ->
                case_of_case if_cond (fun tail ->
                    execute_if
                      [ { if_cond = tail; if_body; if_attrs; if_loc_then } ]
                      else_ ~loc:expr.pexp_loc ~can_simplify)
                |> Option.value ~default:expr
            | { pexp_desc = Pexp_match (matched, cases); _ } ->
                case_of_case matched (fun tail ->
                    execute_match tail (expr.pexp_loc, cases) ~can_simplify)
                |> Option.value ~default:expr
            | { pexp_desc = Pexp_apply (fun_, args); _ } -> (
                match
                  execute_apply ~process_call:(process_call ~ctx:!ctx) ~loc:expr.pexp_loc
                    fun_ args
                with
                | None -> expr
                | Some v -> v)
            | _ -> expr)
    }
  in
  self

let bind__sequentially_if_possible ~ctx ~type_index bindings body =
  (* We try to make separate let-bindings when doing so won't cause captures.  It'd be
     much simpler to always generate let-and, but it is not common in user written
     code, so it seems better to avoid that if we can. *)
  let body = ref body in
  let rev_bindings = ref (List.rev bindings) in
  let bind bindings (body : P.expression) =
    Ast_helper.Exp.let'
      (List.map bindings ~f:(fun (p, e) ->
           Ast_helper.Vb.mk ~loc:(migrate_loc `Gen) p [] (Pfunction_body e) ~is_pun:false
             ?value_constraint:(value_constraint ~ctx ~type_index e)))
      body ~loc_in:body.pexp_loc ~loc:(migrate_loc `Gen)
  in
  while
    match !rev_bindings with
    | [] -> false
    | last_binding :: rev_bindings_tl ->
        let fvs = free_vars (snd last_binding) in
        let cant_bind_separately =
          exists_from_iter (fun yield ->
              List.iter rev_bindings_tl ~f:(fun (p, _) ->
                  iter_bound_vars __.pat p (fun var -> if Map.mem fvs var then yield ())))
        in
        if cant_bind_separately
        then false
        else (
          body := bind [ last_binding ] !body;
          rev_bindings := rev_bindings_tl;
          true)
  do
    ()
  done;
  bind (List.rev !rev_bindings) !body

let reduce ~ctx ~type_index
    (combined : (P.expr_function_param * function_arg option) list) (body : P.expression)
    : P.expression =
  let bindings =
    List.map combined ~f:(fun (param, arg) ->
        match param.pparam_desc with
        | Pparam_newtype _ -> assert false
        | Pparam_val (plabel, edefault, pattern) -> (
            match arg with
            | None -> (
                match plabel with
                | Asttypes.Optional _ -> (
                    match edefault with
                    | Some e -> (pattern, e)
                    | None -> (pattern, Ast_helper.Exp.none ~loc:(migrate_loc `Gen) ()))
                | _ -> assert false)
            | Some (alabel, expr) -> (
                match (plabel, alabel) with
                | Asttypes.Nolabel, Asttypes.Nolabel -> (pattern, expr)
                | Asttypes.Nolabel, _ | _, Asttypes.Nolabel -> assert false
                | Labelled _, (Optional _ | Labelled _) -> (pattern, expr)
                | Optional _, Optional _ -> (
                    match edefault with
                    | None -> (pattern, expr)
                    | Some default ->
                        ( pattern
                        , Ast_helper.with_default_loc (migrate_loc `Gen) (fun () ->
                              Ast_helper.Exp.match_ expr
                                [ Ast_helper.Exp.case
                                    (Ast_helper.Pat.some
                                       (Ast_helper.Pat.var (Ast_helper.located "x")))
                                    (Ast_helper.Exp.ident' (Lident "x"))
                                ; Ast_helper.Exp.case (Ast_helper.Pat.none ()) default
                                ]) ))
                | Optional _, Labelled _ -> (
                    match edefault with
                    | Some _ -> (pattern, expr)
                    | None -> (pattern, Ast_helper.Exp.some ~loc:(migrate_loc `Gen) expr))
                )))
  in
  let body_fvs, _, body_ambiguous_refs = var_names __.expr body in
  let bindings_to_inline, bindings_to_name =
    let annotate_preferred_name ((p : P.pattern), (e : P.expression)) =
      match (p.ppat_desc, e.pexp_desc) with
      | Ppat_var var, Pexp_ident { txt = Lident e_var; _ }
        when (not (is_migrate_filename p.ppat_loc))
             && is_migrate_filename e.pexp_loc
             && var.txt <>: e_var ->
          ( p
          , { e with
              pexp_attributes =
                (let loc = p.ppat_loc in
                 Sattr.pref.build ~loc
                   (Ast_helper.Exp.ident ~loc ~attrs:p.ppat_attributes
                      { txt = Lident var.txt; loc = var.loc }))
                :: e.pexp_attributes
            } )
      | _ -> (p, e)
    in
    let bindings =
      let i = ref 0 in
      List.map bindings ~f:(fun (p, e) ->
          let e_fvs = free_vars e in
          Int.incr i;
          if
            (* If [p, e] is [x, a + 1], then we make sure that all refs to [a] in the
                 expression can be resolved, because when we substitute [x] by [a + 1],
                 if we find a [let a = .. in ..], we'll alpha convert on the fly. But for
                 that alpha-conversion to succeed, we must be able to reliably know what
                 are the references to [a]. *)
            exists_from_iter (fun yield ->
                Map.iter_keys e_fvs ~f:(fun var ->
                    if Set.mem body_ambiguous_refs var then yield ()))
          then (`No, (p, e))
          else
            match p.ppat_desc with
            | Ppat_var var -> (
                match Map.find body_fvs var.txt with
                | None -> (`No, ({ p with ppat_desc = Ppat_any }, e))
                | Some use -> (
                    match
                      match use with
                      | Ambig -> `Leave_it
                      | More _ -> if duplicatable e then `Subst else `Leave_it
                      | Once ->
                          if
                            execution_commutes e
                            || substitution_would_preserve_execution var.txt body
                          then `Subst
                          else `Leave_it
                    with
                    | `Leave_it -> (`No, (p, e))
                    | `Subst -> (`Maybe (!i, var.txt, e_fvs), (p, e))))
            | _ -> (`No, (p, e)))
    in
    let rec fixpoint bindings =
      let bindings' =
        List.map bindings ~f:(fun binding ->
            match binding with
            | (`No | `Yes _), _ -> binding
            | `Maybe (i, v, fvs), pe ->
                (* [let x = y and y = x] should be inlinable, but that requires a
                   better fixpoint computation than we do here. But maybe we should
                   alpha convert instead of doing some kind of fixpoint. *)
                let exists_capture =
                  exists_from_iter (fun yield ->
                      List.iter bindings ~f:(fun ((_, (p, _)) as binding) ->
                          let inspect =
                            match binding with
                            | `Yes _, _ -> false
                            | `No, _ -> true
                            | `Maybe (j, _, _), _ -> i <> j
                          in
                          if inspect
                          then
                            iter_bound_vars __.pat p (fun var ->
                                if Map.mem fvs var then yield ())))
                in
                if exists_capture then binding else (`Yes v, annotate_preferred_name pe))
      in
      if
        let is_yes = function `Yes _, _ -> true | (`No | `Maybe _), _ -> false in
        List.count bindings ~f:is_yes = List.count bindings' ~f:is_yes
      then
        List.partition_map bindings ~f:(function
          | `Yes v, (_, e) -> First (v, e)
          | (`Maybe _ | `No), pe -> Second pe)
      else fixpoint bindings'
    in
    fixpoint bindings
  in
  let body = substitute (Map.of_alist_exn (module String) bindings_to_inline) body in
  let body =
    match body with
    | { pexp_desc = Pexp_apply (fun_, args); _ } ->
        if Attr.exists body.pexp_attributes (Attr.reorder `Internal)
        then body
        else
          (* We assume we can reorder all arguments. If not, the original code should
             add parens to make it clear there is an overapplication, or something. *)
          { body with pexp_desc = Pexp_apply (fun_, commute_args args) }
    | _ -> body
  in
  bind__sequentially_if_possible ~ctx ~type_index bindings_to_name body

let process_call ~ctx ~type_index ~apply_loc args (to_expr : P.expression) =
  match to_expr.pexp_desc with
  | Pexp_function (params, None, Pfunction_body body) -> (
      match line_up_params_and_args ~ctx ~apply_loc params args body with
      | None -> None
      | Some (combined, body) -> Some (reduce ~ctx ~type_index combined body))
  | _ -> None

let find_attribute_payload_uast ~fmconf ?repl (attributes : Typedtree.attributes) =
  List.find_map attributes ~f:(function
    | { attr_name = { txt = "migrate"; _ }
      ; attr_payload = PStr [ { pstr_desc = Pstr_eval (e, _); _ } ]
      ; _
      } ->
        Some
          (attribute_payload
             ?repl:(Option.map repl ~f:Lazy.force)
             ~loc:(Conv.location e.pexp_loc)
             (Some (fmexpr_of_uexpr ~fmconf `Import e)))
    | { attr_name = { txt = "migrate"; _ }; attr_payload = PStr []; attr_loc; _ } ->
        Some
          (attribute_payload
             ?repl:(Option.map repl ~f:Lazy.force)
             ~loc:(Conv.location attr_loc) None)
    | _ -> None)

let find_attribute_payload_fmast ?repl (attributes : P.attributes) =
  let repl = Option.map repl ~f:(fmexpr_of_fmexpr `Import __) in
  List.find_map attributes ~f:(function
    | { attr_name = { txt = "migrate"; _ }
      ; attr_payload = PStr [ { pstr_desc = Pstr_eval (e, _); _ } ]
      ; _
      } ->
        Some (attribute_payload ?repl ~loc:e.pexp_loc (Some (fmexpr_of_fmexpr `Import e)))
    | { attr_name = { txt = "migrate"; _ }; attr_payload = PStr []; attr_loc; _ } ->
        Some (attribute_payload ?repl ~loc:attr_loc None)
    | _ -> None)

let remove_attribute_payload_fmast (attributes : P.attributes) =
  List.filter attributes ~f:(function
    | { attr_name = { txt = "migrate"; _ }; _ } -> false
    | _ -> true)

let update_attribute_payload_fmast (attributes : P.attributes) payload =
  List.map attributes ~f:(function
    | { attr_name = { txt = "migrate"; _ }; attr_payload; _ } as attr ->
        let attrs =
          match attr_payload with
          | PStr [ { pstr_desc = Pstr_eval (_, attrs); _ } ] -> attrs
          | _ -> []
        in
        { attr with attr_payload = PStr (payload_attribute ~attrs payload) }
    | z -> z)

let find_extra_migration_uast ~fmconf (e : Typedtree.expression) =
  match e.exp_desc with
  | Texp_ident (_, id, _) -> (
      match find_attribute_payload_uast ~fmconf e.exp_attributes with
      | None -> None
      | Some payload -> Some (e, id, payload))
  | Texp_tuple [ ({ exp_desc = Texp_ident (_, id1, _); _ } as e1); e2 ]
  | Texp_construct
      ( { txt = Lident "::"; _ }
      , _
      , [ ({ exp_desc = Texp_ident (_, id1, _); _ } as e1)
        ; { exp_desc =
              Texp_construct
                ( { txt = Lident "::"; _ }
                , _
                , [ e2
                  ; { exp_desc = Texp_construct ({ txt = Lident "[]"; _ }, _, []); _ }
                  ] )
          ; _
          }
        ] ) -> (
      (* Probably bad, we should probably just remove this. In fact, even if we could
         deal with desugaring like filling in of optional arguments, there's no way we
         could undo ppx expansions. So the only good option here would be to find the
         source code, rather than the cmt. *)
      let repl =
        lazy (fmexpr_of_uexpr ~fmconf `Import (Untypeast.untype_expression e2))
      in
      match find_attribute_payload_uast ~fmconf ~repl e.exp_attributes with
      | None -> None
      | Some payload -> Some (e1, id1, payload))
  | _ -> None

let find_extra_migration_fmast (e : P.expression) =
  match e.pexp_desc with
  | Pexp_ident id -> (
      match find_attribute_payload_fmast e.pexp_attributes with
      | None -> None
      | Some payload -> Some (e, id, `Id, payload))
  | Pexp_tuple [ ({ pexp_desc = Pexp_ident id1; _ } as e1); e2 ]
  | Pexp_list [ ({ pexp_desc = Pexp_ident id1; _ } as e1); e2 ] -> (
      (* The problem with these syntaxes is that you can't use [%context] and Rel.
         Maybe that's ok, but not sure. At least the fact that it's typed is less
         heavy, and the use of Rel is perhaps only meaningful for inline migrations,
         not sure if it's really useful for side migrations. *)
      match find_attribute_payload_fmast ~repl:e2 e.pexp_attributes with
      | None -> None
      | Some payload -> Some (e1, id1, `Structure, payload))
  | _ -> None

let value_decl_of_item_decl ~context ~id v =
  match v with
  | Error s ->
      if !log || debug.all
      then
        print_s
          [%sexp
            (id : Uast.Longident.t)
          , "shape lookup error"
          , (s : string Lazy.t)
          , (context : string)];
      None
  | Ok (uid, None) -> Some (`Nf uid)
  | Ok (_uid, Some (item_declaration : Typedtree.item_declaration)) -> (
      match item_declaration with
      | Value vd ->
          if !log || debug.all
          then print_s [%sexp (id : Uast.Longident.t), "shape lookup: found prim"];
          Some (`Prim vd)
      | Value_binding vb ->
          if !log || debug.all
          then print_s [%sexp (id : Uast.Longident.t), "shape lookup: found value"];
          Some (`Let vb)
      | _ ->
          if !log || debug.all
          then
            print_s
              [%sexp
                (id : Uast.Longident.t)
              , "shape lookup: found unexpected"
              , (match item_declaration with
                 | Value _ -> "value"
                 | Value_binding _ -> "value_binding"
                 | Type _ -> "type"
                 | Constructor _ -> "constructor"
                 | Extension_constructor _ -> "extension_constructor"
                 | Label _ -> "label"
                 | Module _ -> "module"
                 | Module_substitution _ -> "module_substitution"
                 | Module_binding _ -> "module_binding"
                 | Module_type _ -> "module_type"
                 | Class _ -> "class"
                 | Class_type _ -> "class_type"
                  : string)];
          None)

let decl_from_id_uast ~context ~artifacts
    (comp_unit, (id : Uast.Longident.t Uast.Location.loc)) =
  match Build.Artifacts.shape_from_occurrence artifacts (comp_unit, id) with
  | None ->
      if !log || debug.all
      then
        print_s
          [%sexp
            (id.txt : Uast.Longident.t), "shape lookup: None", (id.loc : Uast.Location.t)];
      None
  | Some v -> value_decl_of_item_decl ~context ~id:id.txt v

let decl_from_id_fmast ~context ~artifacts (comp_unit, id) =
  decl_from_id_uast ~context ~artifacts (comp_unit, Conv.located' Conv.longident' id)

module Decl_id = struct
  type shape_uid = Uast.Shape.Uid.t [@@deriving sexp_of]

  let compare_shape_uid (t1 : shape_uid) (t2 : shape_uid) =
    match (t1, t2) with
    | ( Item { comp_unit = c1; id = id1; from = from1 }
      , Item { comp_unit = c2; id = id2; from = from2 } ) ->
        [%compare: string * int * Uast.Unit_info.intf_or_impl] (c1, id1, from1)
          (c2, id2, from2)
    | _ -> assert false

  let hash_fold_shape_uid acc (t : shape_uid) =
    match t with
    | Item { comp_unit; id; from } ->
        [%hash_fold: string * int * Uast.Unit_info.intf_or_impl] acc (comp_unit, id, from)
    | _ -> assert false

  type t =
    | Vb of Uast.Location.Including_filename.t * string
    | Nf of shape_uid
  [@@deriving compare, hash, sexp_of]

  let create id = function
    | `Nf uid -> Nf uid
    | (`Prim _ | `Let _) as vb ->
        Vb
          ( (match vb with
            | `Prim (prim : Typedtree.value_description) -> prim.val_loc
            | `Let (vb : Typedtree.value_binding) -> vb.vb_loc)
          , Longident.last id )
end

(* A.B.C.d -> Ldot (Ldot (Ldot (Ldot (Lident A), B), C), d)
   F(M).t -> Ldot (Lapply (F, M)) t
*)
let rec rel_opt : type a. a Uast.ns -> _ =
 fun ns ~resolved_modpath (l1 : Longident.t) (l2 : Longident.t) : Longident.t option ->
  match l2 with
  | Lident s ->
      if s =: "Rel"
      then
        match l1 with
        | Lident _ -> None
        | Ldot (base_mod, _) -> Some base_mod
        | Lapply _ -> assert false
      else Some l2
  | Ldot (l2, s) ->
      Some
        (match rel_opt ns ~resolved_modpath l1 l2 with
        | None -> Lident s
        | Some l2' -> Ldot (l2', s))
  | Lapply (f, arg) ->
      Some
        (Lapply (rel Module ~resolved_modpath l1 f, rel Module ~resolved_modpath l1 arg))

and rel : type a. a Uast.ns -> _ =
 fun ns ~resolved_modpath l1 l2 ->
  match rel_opt ns ~resolved_modpath l1 l2 with
  | Some l -> (
      match resolved_modpath with
      | None -> l
      | Some (lazy (modlid, env)) -> (
          (* Prevent shadowing of all components of l2, except the first one *)
          match rel_opt ns ~resolved_modpath modlid l2 with
          | None -> assert false
          | Some l' -> (
              match Uast.without_type_based_disambiguation ns with
              | None -> l
              | Some (T T) -> (
                  match Requalify.same_resolution ns (l', env) (l, env) with
                  | `Unknown | `Yes -> l
                  | `No _ -> l'))))
  | None -> Lident "Nomodulename"

let relativize ~resolved_modpath path meth e =
  let super = Ast_mapper.default_mapper in
  let self =
    { super with
      module_expr =
        (fun self mexpr ->
          let mexpr = super.module_expr self mexpr in
          match mexpr.pmod_desc with
          | Pmod_ident { txt = var; loc } ->
              let var' = rel Module ~resolved_modpath path var in
              { mexpr with pmod_desc = Pmod_ident { txt = var'; loc } }
          | _ -> mexpr)
    ; expr =
        (fun self expr ->
          let expr = super.expr self expr in
          match expr with
          | { pexp_desc = Pexp_ident { txt = var; loc }; _ } ->
              let var' = rel Value ~resolved_modpath path var in
              { expr with pexp_desc = Pexp_ident { txt = var'; loc } }
          | { pexp_desc = Pexp_construct ({ txt = var; loc }, arg); _ } ->
              let var' = rel Constructor ~resolved_modpath path var in
              { expr with pexp_desc = Pexp_construct ({ txt = var'; loc }, arg) }
          | { pexp_desc = Pexp_record (fields, arg); _ } ->
              let fields' =
                List.map fields ~f:(fun ({ txt = var; loc }, a, b) ->
                    let var' = rel Label ~resolved_modpath path var in
                    (({ txt = var'; loc } : _ Location.loc), a, b))
              in
              { expr with pexp_desc = Pexp_record (fields', arg) }
          | _ -> expr)
    ; typ =
        (fun self v ->
          let v = super.typ self v in
          match v with
          | { ptyp_desc = Ptyp_constr (id, args); _ } ->
              let id' = rel Type ~resolved_modpath path id.txt in
              { v with ptyp_desc = Ptyp_constr ({ id with txt = id' }, args) }
          | _ -> v)
    }
  in
  (meth self) self e

let payload_from_val_fmast ~fmconf ~type_index (id, ident_loc) =
  match Build.Type_index.exp type_index (Conv.location' ident_loc) with
  | { exp_desc = Texp_ident (_, _, vd); _ } :: _ -> (
      match find_attribute_payload_uast ~fmconf vd.val_attributes with
      | None ->
          if !log || debug.all
          then print_s [%sexp (id : Longident.t), "no side migration, no attr on val"];
          None
      | Some _ as opt ->
          if !log || debug.all
          then print_s [%sexp (id : Longident.t), "found attribute on val"];
          opt)
  | _ -> None

let payload_from_type_decl_fmast ~fmconf ~type_index ~id_for_logging:id v =
  match Build.Type_index.typ type_index (Conv.location' v) with
  | { ctyp_desc = Ttyp_constr (path, _, _); ctyp_env; _ } :: _ -> (
      let env = Envaux.env_of_only_summary ctyp_env in
      let type_decl = Env.find_type path env in
      match find_attribute_payload_uast ~fmconf type_decl.type_attributes with
      | None ->
          if !log || debug.all
          then print_s [%sexp (id : Longident.t), "no side migration, no attr on type"];
          None
      | Some attr ->
          if !log || debug.all
          then print_s [%sexp (id : Longident.t), "found attribute on type"];
          Some attr)
  | _ -> None

let payload_from_exn_decl_fmast ~fmconf ~type_index ~id_for_logging:id (nsv, v)
    ~construct_of =
  match Build.Type_index.find type_index nsv v with
  | [] -> None
  | elt :: _ -> (
      match construct_of elt with
      | None -> None
      | Some (construct_desc : Types.constructor_description) -> (
          match construct_desc.cstr_tag with
          | Cstr_extension _ -> (
              match
                find_attribute_payload_uast ~fmconf construct_desc.cstr_attributes
              with
              | None ->
                  if !log || debug.all
                  then
                    print_s
                      [%sexp (id : Longident.t), "no side migration, no attr on exn"];
                  None
              | Some attr ->
                  if !log || debug.all
                  then print_s [%sexp (id : Longident.t), "found attribute on exn"];
                  Some attr)
          | _ -> None))

let rec find_map_lident_prefix (lid : Longident.t) f =
  match lid with
  | Lident _ | Lapply _ -> f lid
  | Ldot (left, right) -> (
      match find_map_lident_prefix left f with
      | None -> f lid
      | Some res -> Some (Longident.Ldot (res, right)))

let find_map_lident_prefix ~start lid f =
  match start with
  | `Here -> find_map_lident_prefix lid f
  | `Parent -> (
      match lid with
      | Lident _ | Lapply _ -> None
      | Ldot (left, right) -> (
          match find_map_lident_prefix left f with
          | None -> None
          | Some res -> Some (Longident.Ldot (res, right))))

let find_module_decl_payload (type a b) ~fmconf ~type_index ~module_migrations
    ((nsv : (_, b, _, _) Fmast.Node.t), v) ((lidns : a Uast.ns), (lid : Longident.t))
    ~build ~changed_something =
  (* A few points that might be worth improving:
     - this being opt-in. Maybe this can be sped up, or ocamlmig in general be sped up,
     so we don't care about speed.
     - we should support `Rel.`, and maybe Requalify.requalify (although is this doing
     something in cases other than shadowing the Stdlib?)
     - this only supports 'module M : sig ... end [@@migrate]', not definitions on
     implementations.
     - not all namespaces are implemented.
   *)
  if not module_migrations
  then None
  else
    let env =
      lazy
        (match Build.Type_index.find type_index nsv v with
        | [] ->
            if !log || debug.all
            then print_s [%sexp (lid : Longident.t), "no type information"];
            None
        | res :: _ -> Some (Envaux.env_of_only_summary (Build.Type_index.env nsv res)))
    in
    find_map_lident_prefix
      ~start:(match lidns with Module -> `Here | _ -> `Parent)
      lid
      (fun lid ->
        match force env with
        | None -> None
        | Some env -> (
            match Uast.find_by_name Module env (Conv.longident' lid) with
            | exception Stdlib.Not_found ->
                if !log || debug.all
                then print_s [%sexp (lid : Longident.t), "module not in env"];
                None
            | _path, md -> (
                match find_attribute_payload_uast ~fmconf md.md_attributes with
                | None ->
                    if !log || debug.all
                    then print_s [%sexp (lid : Longident.t), "no attributes"];
                    None
                | Some payload -> (
                    match payload.repl with
                    | { pexp_desc = Pexp_construct (repl_lid, None); _ } ->
                        Requalify.try_unqualifying_ident (Env.summary env) repl_lid.txt
                          ~same_resolution_as_initially:(fun new_lid ->
                            match
                              Requalify.same_resolution Module (repl_lid.txt, env)
                                (new_lid, env)
                            with
                            | `Yes -> true
                            | `No _ | `Unknown -> false)
                        |> Some __
                    | _ ->
                        if !log || debug.all
                        then print_s [%sexp (lid : Longident.t), "repl is not a path"];
                        None))))
    |> Option.map ~f:(fun flid ->
           changed_something := true;
           Fmast.Node.update nsv v ~desc:(build flid))

let payload_from_occurence_fmast ~fmconf ~context ~artifacts ~extra_migrations
    (comp_unit, id) =
  match decl_from_id_fmast ~context ~artifacts (comp_unit, id) with
  | None -> None
  | Some vb -> (
      let decl_id =
        match id.txt with Lapply _ -> None | _ -> Some (Decl_id.create id.txt vb)
      in
      match Option.bind decl_id ~f:(Hashtbl.find extra_migrations) with
      | Some _ as opt ->
          if !log || debug.all
          then
            print_s
              [%sexp
                (id.txt : Longident.t)
              , "found side migration"
              , (opt : (migrate_payload * _ option) option)];
          opt
      | None -> (
          match
            find_attribute_payload_uast ~fmconf
              (match vb with
              | `Nf _ -> []
              | `Prim prim -> prim.val_attributes
              | `Let vb -> vb.vb_attributes)
          with
          | None ->
              if !log || debug.all
              then
                print_s
                  [%sexp
                    (id.txt : Longident.t)
                  , "no side migration, no attr on def"
                  , (decl_id : Decl_id.t option)];
              None
          | Some _ as opt ->
              if !log || debug.all
              then print_s [%sexp (id.txt : Longident.t), "found attribute on def"];
              Option.map opt ~f:(fun x -> (x, None))))

let load_extra_migrations ~cmts ~artifacts ~fmconf =
  let extra_migrations = Hashtbl.create (module Decl_id) in
  let add_extra_migration_fmast ~type_index ~comp_unit (expr : P.expression) =
    match find_extra_migration_fmast expr with
    | Some (_src, src_id, _, payload) -> (
        match
          decl_from_id_fmast ~context:"adding inline side migration" ~artifacts
            (comp_unit, src_id)
        with
        | None -> ()
        | Some vb ->
            Hashtbl.set extra_migrations
              ~key:(Decl_id.create src_id.txt vb)
              ~data:(payload, type_index))
    | _ -> ()
  in
  Option.iter cmts ~f:(fun (cmt_path, (cmt_infos : Cmt_format.cmt_infos)) ->
      match
        Option.bind cmt_infos.cmt_sourcefile ~f:(fun sourcefile ->
            let source_basename = Filename.basename sourcefile in
            let cmt_dir = Filename.dirname (Cwdpath.to_string cmt_path) in
            let ( ^/ ) = Filename.concat in
            List.find_map
              (* In opam, I'm assuming the .ml would be next to the .cmt, but in dune
                 build dirs, they seem to be two dirs up. *)
              [ cmt_dir; cmt_dir ^/ ".."; cmt_dir ^ ".." ^/ ".." ]
              ~f:(fun dir ->
                let f = dir ^/ source_basename in
                if Sys.file_exists f then Some f else None))
      with
      | Some source_path ->
          let type_index =
            Build.Type_index.create_without_setting_up_loadpath cmt_infos
          in
          (* This branch is to handle expressions like let _ = [ id; repl ] [@@migrate],
            so we have the parsetree of repl, instead of the typedtree, which can be
            different from the syntax (optional arguments filled in for instance). *)
          let source_contents = In_channel.read_all source_path in
          let structure =
            Fmast.parse_with_ocamlformat ~conf:fmconf ~input_name:source_path Structure
              source_contents
          in
          let super = Ast_mapper.default_mapper in
          let self =
            { super with
              expr =
                (fun self expr ->
                  add_extra_migration_fmast ~type_index:(Some type_index)
                    ~comp_unit:cmt_infos.cmt_modname expr;
                  super.expr self expr)
            }
          in
          ignore (self.structure self structure.ast)
      | None -> (
          let super = Tast_iterator.default_iterator in
          let self =
            { super with
              expr =
                (fun self expr ->
                  (match find_extra_migration_uast ~fmconf expr with
                  | Some (_src, src_id, repl) -> (
                      match
                        decl_from_id_uast ~context:"adding side migration" ~artifacts
                          (cmt_infos.cmt_modname, src_id)
                      with
                      | None -> ()
                      | Some vb ->
                          Hashtbl.set extra_migrations
                            ~key:(Decl_id.create (Conv.longident src_id.txt) vb)
                            ~data:(repl, None))
                  | None -> ());
                  super.expr self expr)
            }
          in
          match cmt_infos.cmt_annots with
          | Implementation structure -> self.structure self structure
          | _ -> failwith "cmt didn't contains what's expected"));
  if debug.extra_migrations
  then
    print_s
      [%sexp ~~(extra_migrations : (migrate_payload * _ option) Hashtbl.M(Decl_id).t)];
  (extra_migrations, add_extra_migration_fmast)

let requalify ((expr : P.expression), expr_type_index) new_base_env =
  if !log || debug.all then print_s [%sexp `requalify (expr : Fmast.expression)];
  match Build.Type_index.exp expr_type_index (Conv.location' expr.pexp_loc) with
  | [] -> expr
  | texp :: _ ->
      let rebased_env =
        Uast.Env_summary.rebase' ~old_base:texp.exp_env ~new_base:new_base_env |> __.next
      in
      let self =
        let super = Ast_mapper.default_mapper in
        { super with
          expr =
            (fun self expr ->
              let expr = super.expr self expr in
              match expr with
              | { pexp_desc = Pexp_ident { txt = var; loc }; _ } -> (
                  match
                    Build.Type_index.exp expr_type_index (Conv.location' expr.pexp_loc)
                  with
                  | [] -> expr
                  | texp :: _ ->
                      let orig_env = Envaux.env_of_only_summary texp.exp_env in
                      let rebased_env = rebased_env texp.exp_env in
                      let var' =
                        var
                        |> Requalify.requalify Value orig_env rebased_env __
                        |> Requalify.try_unqualifying_ident (Env.summary new_base_env) __
                             ~same_resolution_as_initially:(fun new_var ->
                               match
                                 Requalify.same_resolution Value (var, orig_env)
                                   (new_var, rebased_env)
                               with
                               | `Yes -> true
                               | `No _ | `Unknown -> false)
                      in
                      { expr with pexp_desc = Pexp_ident { txt = var'; loc } })
              | _ -> expr)
        }
      in
      self.expr self expr

let resolved_modpath ~type_index nsv v ~path_of =
  match Build.Type_index.find type_index nsv v with
  | [] -> None
  | elt :: _ -> (
      match path_of elt with
      | None -> None
      | Some path ->
          let env = Build.Type_index.env nsv elt in
          Some
            (lazy
              (let env = Envaux.env_of_only_summary env in
               let ident =
                 path
                 |> Requalify.ident_of_path_exn __
                 |> Longident.map_modpath __ (fun modpath ->
                        Requalify.try_unqualifying_ident
                          ~same_resolution_as_initially:(fun new_var ->
                            match
                              Requalify.same_resolution Module (modpath, env)
                                (new_var, env)
                            with
                            | `Yes -> true
                            | `No _ | `Unknown -> false)
                          (Env.summary env) modpath)
               in
               (ident, env))))

let inline ~fmconf ~type_index ~extra_migrations_cmts ~artifacts:(comp_unit, artifacts)
    ~changed_something ~add_depends ~ctx =
  let extra_migrations, add_extra_migration_fmast =
    load_extra_migrations ~cmts:extra_migrations_cmts ~artifacts ~fmconf
  in
  let ocamlformat_disabled = ref false in
  let has_disabled_ocamlformat (si : Parsetree.structure_item) =
    match si.pstr_desc with
    | Pstr_value vbs ->
        List.exists vbs.pvbs_bindings ~f:(fun vb ->
            List.exists (vb.pvb_attributes.attrs_before @ vb.pvb_attributes.attrs_after)
              ~f:(fun attr ->
                attr.attr_name.txt =: "ocamlformat"
                &&
                match attr.attr_payload with
                | PStr [ { pstr_desc = Pstr_eval (e, []); _ } ] -> (
                    match e.pexp_desc with
                    | Pexp_constant { pconst_desc = Pconst_string ("disable", _, _); _ }
                      ->
                        true
                    | _ -> false)
                | _ -> false))
    | _ -> false
  in
  let record_if_ocamlformat_disabled wrapped self si =
    if has_disabled_ocamlformat si
    then Ref.set_temporarily ocamlformat_disabled true ~f:(fun () -> wrapped self si)
    else wrapped self si
  in
  let warn_about_disabled_ocamlformat () =
    if !log && !ocamlformat_disabled
    then
      print_s
        [%sexp
          "found occurrence of rewritten identifier under [@@ocamlformat \"disable\"]. \
           The ocamlformat printer will ignore all modifications made."]
  in
  let super = Ast_mapper.default_mapper in
  { super with
    expr =
      with_log (fun self v ->
          let v = super.expr self v in
          let v' =
            match v.pexp_desc with
            | Pexp_ident id -> (
                match
                  match
                    payload_from_val_fmast ~fmconf ~type_index (id.txt, v.pexp_loc)
                  with
                  | Some _ as opt -> Option.map opt ~f:(fun x -> (x, None))
                  | None ->
                      payload_from_occurence_fmast ~fmconf ~context:"lookup migration"
                        ~artifacts ~extra_migrations (comp_unit, id)
                with
                | None ->
                    find_module_decl_payload ~fmconf ~type_index
                      ~module_migrations:ctx.module_migrations (Exp, v) (Value, id.txt)
                      ~build:(fun newlid -> Pexp_ident { id with txt = newlid })
                      ~changed_something
                    |> Option.value ~default:v
                | Some ({ repl = to_expr; libraries }, repl_type_index) ->
                    warn_about_disabled_ocamlformat ();
                    add_depends libraries;
                    let to_expr =
                      match repl_type_index with
                      | None -> to_expr
                      | Some repl_type_index -> (
                          match
                            Build.Type_index.exp type_index (Conv.location' v.pexp_loc)
                          with
                          | texp :: _ -> requalify (to_expr, repl_type_index) texp.exp_env
                          | [] -> to_expr)
                    in
                    let to_expr =
                      relativize id.txt __.expr to_expr
                        ~resolved_modpath:
                          (resolved_modpath ~type_index Exp v ~path_of:(function
                            | { exp_desc = Texp_ident (path, _, _); _ } -> Some path
                            | _ -> None))
                    in
                    let to_expr =
                      preserve_loc_to_preserve_comment_pos_expr ~from:v to_expr
                    in
                    changed_something := true;
                    if has_context_match to_expr
                    then
                      { to_expr with
                        pexp_attributes =
                          Sattr.orig.build ~loc:!Ast_helper.default_loc v
                          :: to_expr.pexp_attributes
                      }
                    else to_expr)
            | Pexp_construct (id, body) -> (
                match
                  payload_from_exn_decl_fmast ~fmconf ~type_index ~id_for_logging:id.txt
                    (Exp, v) ~construct_of:(function
                    | { exp_desc = Texp_construct (_, desc, _); _ } -> Some desc
                    | _ -> None)
                with
                | Some { repl = { pexp_desc = Pexp_construct (id2, None); _ }; libraries }
                  ->
                    warn_about_disabled_ocamlformat ();
                    add_depends libraries;
                    let id2 =
                      { id2 with
                        txt =
                          rel Constructor id.txt id2.txt
                            ~resolved_modpath:
                              (resolved_modpath ~type_index Exp v ~path_of:(function
                                | { exp_desc =
                                      Texp_construct
                                        (_, { cstr_tag = Cstr_extension (path, _); _ }, _)
                                  ; _
                                  } ->
                                    Some path
                                | _ -> None))
                      }
                    in
                    changed_something := true;
                    { v with
                      pexp_desc = Pexp_construct (id2, body)
                    ; pexp_attributes =
                        Sattr.touched.build ~loc:!Ast_helper.default_loc ()
                        :: v.pexp_attributes
                    }
                | _ ->
                    find_module_decl_payload ~fmconf ~type_index
                      ~module_migrations:ctx.module_migrations (Exp, v)
                      (Constructor, id.txt)
                      ~build:(fun newid -> Pexp_construct ({ id with txt = newid }, body))
                      ~changed_something
                    |> Option.value ~default:v)
            | _ -> v
          in
          (* After the lookup, so we don't replace the definition itself. *)
          add_extra_migration_fmast ~type_index:(Some type_index) ~comp_unit v;
          v')
  ; pat =
      (fun self v ->
        let v = super.pat self v in
        match v.ppat_desc with
        | Ppat_construct (id, body) -> (
            match
              payload_from_exn_decl_fmast ~fmconf ~type_index ~id_for_logging:id.txt
                (Pat, v) ~construct_of:(function
                | T { pat_desc = Tpat_construct (_, desc, _, _); _ } -> Some desc
                | _ -> None)
            with
            | Some { repl = { pexp_desc = Pexp_construct (id2, None); _ }; libraries } ->
                warn_about_disabled_ocamlformat ();
                add_depends libraries;
                let id2 =
                  { id2 with
                    txt =
                      rel Constructor id.txt id2.txt
                        ~resolved_modpath:
                          (resolved_modpath ~type_index Pat v ~path_of:(function
                            | T
                                { pat_desc =
                                    Tpat_construct
                                      (_, { cstr_tag = Cstr_extension (path, _); _ }, _, _)
                                ; _
                                } ->
                                Some path
                            | _ -> None))
                  }
                in
                changed_something := true;
                { v with
                  ppat_desc = Ppat_construct (id2, body)
                ; ppat_attributes =
                    Sattr.touched.build ~loc:!Ast_helper.default_loc ()
                    :: v.ppat_attributes
                }
            | _ ->
                find_module_decl_payload ~fmconf ~type_index
                  ~module_migrations:ctx.module_migrations (Pat, v) (Constructor, id.txt)
                  ~build:(fun newid -> Ppat_construct ({ id with txt = newid }, body))
                  ~changed_something
                |> Option.value ~default:v)
        | _ -> v)
  ; module_expr =
      (fun self v ->
        let v = super.module_expr self v in
        match v.pmod_desc with
        | Pmod_ident id ->
            find_module_decl_payload ~fmconf ~type_index
              ~module_migrations:ctx.module_migrations (Mexp, v) (Module, id.txt)
              ~build:(fun newlid -> Pmod_ident { id with txt = newlid })
              ~changed_something
            |> Option.value ~default:v
        | _ -> v)
  ; typ =
      (fun self v ->
        let v = super.typ self v in
        match v.ptyp_desc with
        | Ptyp_constr (id, args) -> (
            match
              payload_from_type_decl_fmast ~fmconf ~type_index ~id_for_logging:id.txt
                v.ptyp_loc
            with
            | Some { repl = { pexp_desc = Pexp_ident id2; _ }; libraries } ->
                warn_about_disabled_ocamlformat ();
                add_depends libraries;
                let id2 =
                  { id2 with
                    txt =
                      rel Type id.txt id2.txt
                        ~resolved_modpath:
                          (resolved_modpath ~type_index Typ v ~path_of:(function
                            | { ctyp_desc = Ttyp_constr (path, _, _); _ } -> Some path
                            | _ -> None))
                  }
                in
                changed_something := true;
                { v with
                  ptyp_desc = Ptyp_constr (id2, args)
                ; ptyp_attributes =
                    Sattr.touched.build ~loc:!Ast_helper.default_loc ()
                    :: v.ptyp_attributes
                }
            | _ ->
                find_module_decl_payload ~fmconf ~type_index
                  ~module_migrations:ctx.module_migrations (Typ, v) (Type, id.txt)
                  ~build:(fun newid -> Ptyp_constr ({ id with txt = newid }, args))
                  ~changed_something
                |> Option.value ~default:v)
        | Ptyp_class (id, args) ->
            find_module_decl_payload ~fmconf ~type_index
              ~module_migrations:ctx.module_migrations (Typ, v) (Class, id.txt)
              ~build:(fun newid -> Ptyp_class ({ id with txt = newid }, args))
              ~changed_something
            |> Option.value ~default:v
        | _ -> v)
  ; structure_item =
      record_if_ocamlformat_disabled
        (update_migrate_test_payload ~changed_something super |> __.next)
  }

let resolve_idents () =
  let new_id =
    let r = ref (-1) in
    fun () ->
      r := !r + 1;
      !r
  in
  let info_by_id = Hashtbl.create (module Int) in
  let scope = ref (Map.empty (module String)) in
  let _, ctx, super = mapper_with_bound_vars ((fun () -> !scope), (scope := __)) in
  let make_attr id = Sattr.id.build ~loc:!Ast_helper.default_loc id in
  let found_var var attributes =
    let ambig, _, bound_since_open = !ctx in
    if Set.mem bound_since_open var
    then (
      let id = Map.find_exn !scope var in
      (match Sattr.find Sattr.pref attributes with
      | None -> ()
      | Some e ->
          Hashtbl.update info_by_id id ~f:(function
            | Some `Ok -> (
                match e.pexp_desc with
                | Pexp_ident { txt = Lident var; loc } ->
                    `Pref ({ Location.txt = var; loc }, e)
                | _ -> assert false)
            | None -> assert false
            | Some ((`Ambig | `Pref _) as current) -> current));
      Some (make_attr id))
    else if ambig
    then (
      (match Map.find !scope var with
      | None -> ()
      | Some id -> Hashtbl.set info_by_id ~key:id ~data:`Ambig);
      None)
    else None
  in
  let from_current_pat = ref None in
  let bind_pat var =
    let bound = Option.value_exn !from_current_pat in
    match Map.find bound var with
    | Some attr -> attr
    | None ->
        let id = new_id () in
        let attr = make_attr id in
        from_current_pat := Some (Map.add_exn bound ~key:var ~data:attr);
        scope := Map.set !scope ~key:var ~data:id;
        Hashtbl.add_exn info_by_id ~key:id ~data:`Ok;
        attr
  in
  ( { super with
      pat =
        (fun self pat ->
          let unset =
            match !from_current_pat with
            | Some _ -> false
            | None ->
                let bound = Map.empty (module String) in
                from_current_pat := Some bound;
                true
          in
          let pat = super.pat self pat in
          let pat =
            match pat with
            | { ppat_desc = Ppat_var p; _ } ->
                { pat with ppat_attributes = bind_pat p.txt :: pat.ppat_attributes }
            | { ppat_desc = Ppat_alias (_, p); _ } ->
                { pat with ppat_attributes = bind_pat p.txt :: pat.ppat_attributes }
            | _ -> pat
          in
          if unset then from_current_pat := None;
          pat)
    ; expr =
        (fun self expr ->
          let expr =
            match expr.pexp_desc with
            | Pexp_ident { txt = Lident var; _ } -> (
                match found_var var expr.pexp_attributes with
                | None -> expr
                | Some attr ->
                    { expr with pexp_attributes = attr :: expr.pexp_attributes })
            | _ -> expr
          in
          super.expr self expr)
    }
  , info_by_id )

let use_preferred_names (type a) (file_type : a File_type.t) structure =
  let _, _, body_ambiguous_refs = var_names (File_type.method_ file_type) structure in
  let annotate, info_by_id = resolve_idents () in
  let structure = File_type.map file_type annotate structure in
  let super = Ast_mapper.default_mapper in
  let find_id attrs =
    match Sattr.find Sattr.id attrs with
    | None -> None
    | Some id -> (
        match Hashtbl.find_exn info_by_id id with
        | `Pref (var, e) ->
            if Set.mem body_ambiguous_refs var.txt then None else Some (var, e)
        | _ -> None)
  in
  let self =
    { super with
      pat =
        (fun self pat ->
          let pat =
            match pat with
            | { ppat_desc = Ppat_var p; _ } ->
                let p' =
                  match find_id pat.ppat_attributes with Some (var, _) -> var | _ -> p
                in
                { pat with ppat_desc = Ppat_var p' }
            | { ppat_desc = Ppat_alias (subpat, p); _ } ->
                let p' =
                  match find_id pat.ppat_attributes with Some (var, _) -> var | _ -> p
                in
                { pat with ppat_desc = Ppat_alias (subpat, p') }
            | _ -> pat
          in
          super.pat self pat)
    ; expr =
        (fun self expr ->
          let expr =
            match expr.pexp_desc with
            | Pexp_ident { txt = Lident _; _ } -> (
                match find_id expr.pexp_attributes with None -> expr | Some (_, e) -> e)
            | _ -> expr
          in
          super.expr self expr)
    }
  in
  File_type.map file_type self structure

let run ~artifacts ~type_index ~extra_migrations_cmts ~fmconf ~source_path
    ~module_migrations ~input_name_matching_compilation_command =
  process_file' ~fmconf ~source_path ~input_name_matching_compilation_command
    { f =
        (fun changed_something file_type structure ->
          let has_ppx_partial =
            List.mem ~equal:( =: ) (Dune_files.ppx ~path:source_path) "ppx_partial"
          in
          let depends = Queue.create () in
          let ctx = { has_ppx_partial; fmconf; module_migrations } in
          let inline =
            inline ~fmconf ~type_index ~extra_migrations_cmts ~artifacts
              ~changed_something ~add_depends:(Queue.enqueue_all depends) ~ctx
          in
          let simplify = simplify ~ctx ~type_index (process_call ~type_index) in
          let structure =
            structure
            |> File_type.map file_type inline
            |> File_type.map file_type simplify
            |> use_preferred_names file_type
          in
          (structure, { libraries = Queue.to_list depends }))
    }
