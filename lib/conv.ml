open Base

module Fmast = struct
  include Ocamlformat_ocaml_common
  include Ocamlformat_parser_extended
  module P = Parsetree
end

module Uast = struct
  module Location = Location
  module Longident = Longident
  module Asttypes = Asttypes
  module P = Parsetree
  module Ast_helper = Ast_helper
end

module Fmu = struct
  module From = Uast
  module To = Fmast

  let location : From.Location.t -> To.Location.t =
   fun loc ->
    { loc_start = loc.loc_start; loc_end = loc.loc_end; loc_ghost = loc.loc_ghost }

  let located conv_a : _ From.Location.loc -> _ To.Location.loc =
   fun { txt; loc } -> { txt = conv_a txt; loc = location loc }

  let rec longident : From.Longident.t -> To.Longident.t = function
    | Lident s -> Lident s
    | Ldot (t, s) -> Ldot (longident t, s)
    | Lapply (t1, t2) -> Lapply (longident t1, longident t2)

  let arg_label : From.Asttypes.arg_label -> To.Asttypes.arg_label = function
    | Nolabel -> Nolabel
    | Labelled s -> Labelled { txt = s; loc = Fmast.Location.none }
    | Optional s -> Optional { txt = s; loc = Fmast.Location.none }

  let rec expr : From.P.expression -> To.P.expression =
   fun { pexp_desc; pexp_loc; pexp_loc_stack; pexp_attributes } ->
    { pexp_desc = expr_desc pexp_desc
    ; pexp_loc = location pexp_loc
    ; pexp_loc_stack = List.map ~f:location pexp_loc_stack
    ; pexp_attributes = attributes pexp_attributes
    }

  and expr_desc : From.P.expression_desc -> To.P.expression_desc = function
    | Pexp_ident ident -> Pexp_ident (located longident ident)
    | Pexp_constant const -> Pexp_constant (constant const)
    | Pexp_apply (e, l) ->
        Pexp_apply
          (expr e, List.map l ~f:(fun (label, value) -> (arg_label label, expr value)))
    | Pexp_function (params, constr, body) ->
        Pexp_function
          ( List.map params
              ~f:(fun { pparam_loc; pparam_desc } : To.P.expr_function_param ->
                { pparam_loc = location pparam_loc
                ; pparam_desc =
                    (match pparam_desc with
                    | Pparam_val (label, default, p) ->
                        Pparam_val (arg_label label, Option.map ~f:expr default, pat p)
                    | _ -> raise Stdlib.Exit)
                })
          , Option.map constr ~f:(fun _ -> raise Stdlib.Exit)
          , match body with
            | Pfunction_cases (l, loc, attrs) ->
                Pfunction_cases (cases l, location loc, attributes attrs)
            | Pfunction_body body -> Pfunction_body (expr body) )
    | Pexp_construct (id, o) -> Pexp_construct (located longident id, Option.map o ~f:expr)
    | Pexp_record (fields, base) ->
        Pexp_record
          ( List.map fields ~f:(fun (field, value) ->
                (located longident field, None, Some (expr value)))
          , Option.map base ~f:expr )
    | Pexp_tuple l -> Pexp_tuple (List.map l ~f:expr)
    | Pexp_variant (s, e) ->
        Pexp_variant
          ( { txt = { txt = s; loc = To.Location.none }; loc = To.Location.none }
          , Option.map e ~f:expr )
    | Pexp_sequence (e1, e2) -> Pexp_sequence (expr e1, expr e2)
    | Pexp_let (rec_flag, vbs, e) ->
        Pexp_let (value_bindings ~rec_flag vbs, expr e, To.Location.none)
    | Pexp_open (decl, e) -> Pexp_letopen (open_infos ~f:module_expr decl, expr e)
    | Pexp_coerce (e, ty_opt, ty) -> Pexp_coerce (expr e, Option.map ty_opt ~f:typ, typ ty)
    | Pexp_letmodule (name, me, e) ->
        Pexp_letmodule (located Fn.id name, [], module_expr me, expr e)
    | Pexp_letop { let_; ands; body } ->
        let binding_op : From.P.binding_op -> To.P.binding_op =
         fun { pbop_op; pbop_pat; pbop_exp; pbop_loc } ->
          { pbop_op = located Fn.id pbop_op
          ; pbop_pat = pat pbop_pat
          ; pbop_exp = expr pbop_exp
          ; pbop_loc = location pbop_loc
          ; pbop_args = []
          ; pbop_is_pun = false
          ; pbop_typ = None
          }
        in
        Pexp_letop
          { let_ = binding_op let_
          ; ands = List.map ands ~f:binding_op
          ; body = expr body
          ; loc_in = To.Location.none
          }
    | Pexp_ifthenelse (e1, e2, e3) ->
        Pexp_ifthenelse
          ( [ { if_cond = expr e1
              ; if_body = expr e2
              ; if_attrs = []
              ; if_loc_then = To.Location.none
              }
            ]
          , Option.map e3 ~f:(fun e -> (expr e, To.Location.none)) )
    | Pexp_while (e1, e2) -> Pexp_while (expr e1, expr e2)
    | Pexp_try (e, l) -> Pexp_try (expr e, cases l)
    | Pexp_match (e, l) -> Pexp_match (expr e, cases l)
    | Pexp_extension (name, p) -> Pexp_extension (located Fn.id name, payload p)
    | Pexp_unreachable -> Pexp_unreachable
    | Pexp_constraint (e, ty) -> Pexp_constraint (expr e, typ ty)
    | Pexp_field (e, f) -> Pexp_field (expr e, located longident f)
    | Pexp_lazy e -> Pexp_lazy (expr e)
    | Pexp_array l -> Pexp_array (List.map l ~f:expr)
    | Pexp_pack me -> Pexp_pack (module_expr me, None)
    | Pexp_setfield _ | Pexp_for _ | Pexp_send _ | Pexp_new _ | Pexp_setinstvar _
    | Pexp_override _ | Pexp_letexception _ | Pexp_assert _ | Pexp_poly _ | Pexp_object _
    | Pexp_newtype _ ->
        raise Stdlib.Exit

  and cases : From.P.case list -> To.P.case list = fun l -> List.map l ~f:case

  and case : From.P.case -> To.P.case =
   fun { pc_lhs; pc_guard; pc_rhs } ->
    { pc_lhs = pat pc_lhs; pc_guard = Option.map ~f:expr pc_guard; pc_rhs = expr pc_rhs }

  and typ : From.P.core_type -> To.P.core_type =
   fun { ptyp_desc; ptyp_loc; ptyp_loc_stack; ptyp_attributes } ->
    { ptyp_desc = typ_desc ptyp_desc
    ; ptyp_loc = location ptyp_loc
    ; ptyp_loc_stack = List.map ptyp_loc_stack ~f:location
    ; ptyp_attributes = attributes ptyp_attributes
    }

  and typ_desc : From.P.core_type_desc -> To.P.core_type_desc = function
    | Ptyp_any -> Ptyp_any
    | Ptyp_var v -> Ptyp_var v
    | Ptyp_arrow (l, ty1, ty2) ->
        Ptyp_arrow
          ( [ { pap_label = arg_label l; pap_loc = To.Location.none; pap_type = typ ty1 } ]
          , typ ty2 )
    | Ptyp_constr (id, params) ->
        Ptyp_constr (located longident id, List.map params ~f:typ)
    | Ptyp_tuple l -> Ptyp_tuple (List.map l ~f:typ)
    | Ptyp_object _ | Ptyp_class _ | Ptyp_alias _ | Ptyp_variant _ | Ptyp_poly _
    | Ptyp_package _ | Ptyp_open _ | Ptyp_extension _ ->
        raise Stdlib.Not_found

  and open_infos : type a b. a From.P.open_infos -> f:(a -> b) -> b To.P.open_infos =
   fun { popen_expr; popen_override; popen_loc; popen_attributes } ~f ->
    { popen_expr = f popen_expr
    ; popen_override = (match popen_override with Override -> Override | Fresh -> Fresh)
    ; popen_loc = location popen_loc
    ; popen_attributes = ext_attributes popen_attributes
    }

  and module_expr : From.P.module_expr -> To.P.module_expr =
   fun { pmod_desc; pmod_loc; pmod_attributes } ->
    { pmod_desc = module_expr_desc pmod_desc
    ; pmod_loc = location pmod_loc
    ; pmod_attributes = attributes pmod_attributes
    }

  and module_expr_desc : From.P.module_expr_desc -> To.P.module_expr_desc = function
    | Pmod_ident i -> Pmod_ident (located longident i)
    | Pmod_structure l -> Pmod_structure (List.map ~f:structure_item l)
    | Pmod_functor _ | Pmod_apply _ | Pmod_apply_unit _ | Pmod_constraint _
    | Pmod_unpack _ | Pmod_extension _ ->
        raise Stdlib.Exit

  and value_bindings ~rec_flag:flag (vbs : From.P.value_binding list) :
      To.P.value_bindings =
    { pvbs_bindings = List.map vbs ~f:value_binding; pvbs_rec = rec_flag flag }

  and value_binding : From.P.value_binding -> To.P.value_binding =
   fun { pvb_pat; pvb_expr; pvb_constraint; pvb_attributes; pvb_loc } ->
    { pvb_pat = pat pvb_pat
    ; pvb_body = Pfunction_body (expr pvb_expr)
    ; pvb_constraint = Option.map pvb_constraint ~f:(fun _ -> raise Stdlib.Exit)
    ; pvb_attributes = ext_attributes pvb_attributes
    ; pvb_loc = location pvb_loc
    ; pvb_args = []
    ; pvb_is_pun = false
    }

  and rec_flag : From.Asttypes.rec_flag -> To.Asttypes.rec_flag = function
    | Recursive -> Recursive
    | Nonrecursive -> Nonrecursive

  and constant : From.P.constant -> To.P.constant =
   fun { pconst_desc; pconst_loc } ->
    { pconst_desc = constant_desc pconst_desc; pconst_loc = location pconst_loc }

  and constant_desc : From.P.constant_desc -> To.P.constant_desc = function
    | Pconst_char c -> Pconst_char (c, Printf.sprintf "%C" c)
    | Pconst_integer (s, c) -> Pconst_integer (s, c)
    | Pconst_string (s, loc, delim) -> Pconst_string (s, location loc, delim)
    | Pconst_float (s, c) -> Pconst_float (s, c)

  and pat : From.P.pattern -> To.P.pattern =
   fun { ppat_desc; ppat_loc; ppat_attributes; ppat_loc_stack } ->
    { ppat_desc = pat_desc ppat_desc
    ; ppat_loc = location ppat_loc
    ; ppat_loc_stack = List.map ~f:location ppat_loc_stack
    ; ppat_attributes = attributes ppat_attributes
    }

  and pat_desc : From.P.pattern_desc -> To.P.pattern_desc = function
    | Ppat_any -> Ppat_any
    | Ppat_var v -> Ppat_var (located Fn.id v)
    | Ppat_construct (id, o) ->
        Ppat_construct
          ( located longident id
          , Option.map o ~f:(fun (types, p) -> (List.map types ~f:(located Fn.id), pat p))
          )
    | Ppat_variant (s, e) ->
        Ppat_variant
          ( { txt = { txt = s; loc = To.Location.none }; loc = To.Location.none }
          , Option.map e ~f:pat )
    | Ppat_tuple l -> Ppat_tuple (List.map l ~f:pat)
    | Ppat_constraint (p, ty) -> Ppat_constraint (pat p, typ ty)
    | Ppat_extension (name, p) -> Ppat_extension (located Fn.id name, payload p)
    | Ppat_alias (p, name) -> Ppat_alias (pat p, located Fn.id name)
    | Ppat_constant const -> Ppat_constant (constant const)
    | Ppat_interval (c1, c2) -> Ppat_interval (constant c1, constant c2)
    | Ppat_record (fields, closed) ->
        Ppat_record
          ( List.map fields ~f:(fun (field, p) ->
                (located longident field, None, Some (pat p)))
          , match closed with Closed -> OClosed | Open -> OOpen To.Location.none )
    | Ppat_or (p1, p2) -> Ppat_or [ pat p1; pat p2 ]
    | Ppat_lazy p -> Ppat_lazy (pat p)
    | Ppat_array _ | Ppat_type _ | Ppat_unpack _ | Ppat_exception _ | Ppat_effect _
    | Ppat_open _ ->
        raise Stdlib.Exit

  and attributes : From.P.attributes -> To.P.attributes = fun l -> List.map l ~f:attribute

  and ext_attributes : From.P.attributes -> To.P.ext_attrs =
   fun l -> { attrs_before = []; attrs_after = attributes l; attrs_extension = None }

  and attribute : From.P.attribute -> To.P.attribute =
   fun { attr_name; attr_payload; attr_loc } ->
    { attr_name = located Fn.id attr_name
    ; attr_payload = payload attr_payload
    ; attr_loc = location attr_loc
    }

  and payload : From.P.payload -> To.P.payload = function
    | PStr l -> PStr (structure l)
    | PSig l -> PSig (signature l)
    | PPat (p, e) -> PPat (pat p, Option.map e ~f:expr)
    | PTyp t -> PTyp (typ t)

  and structure : From.P.structure -> To.P.structure =
   fun l -> List.map l ~f:structure_item

  and signature : From.P.signature -> To.P.signature =
   fun l -> List.map l ~f:signature_item

  and signature_item : From.P.signature_item -> To.P.signature_item =
   fun _ -> raise Stdlib.Exit

  and structure_item : From.P.structure_item -> To.P.structure_item =
   fun { pstr_desc; pstr_loc } ->
    { pstr_desc = structure_item_desc pstr_desc; pstr_loc = location pstr_loc }

  and structure_item_desc : From.P.structure_item_desc -> To.P.structure_item_desc =
    function
    | Pstr_eval (e, attrs) -> Pstr_eval (expr e, attributes attrs)
    | Pstr_value (rec_flag, vbs) -> Pstr_value (value_bindings ~rec_flag vbs)
    | Pstr_primitive _ | Pstr_type _ | Pstr_typext _ | Pstr_exception _ | Pstr_module _
    | Pstr_recmodule _ | Pstr_modtype _ | Pstr_open _ | Pstr_class _ | Pstr_class_type _
    | Pstr_include _ | Pstr_attribute _ | Pstr_extension _ ->
        raise Stdlib.Exit
end

module Ufm = struct
  module From = Fmast
  module To = Uast

  let location : From.Location.t -> To.Location.t =
   fun loc ->
    { loc_start = loc.loc_start; loc_end = loc.loc_end; loc_ghost = loc.loc_ghost }

  let located conv_a : _ From.Location.loc -> _ To.Location.loc =
   fun { txt; loc } -> { txt = conv_a txt; loc = location loc }

  let rec longident : From.Longident.t -> To.Longident.t = function
    | Lident s -> Lident s
    | Ldot (t, s) -> Ldot (longident t, s)
    | Lapply (t1, t2) -> Lapply (longident t1, longident t2)

  let arg_label : From.Asttypes.arg_label -> To.Asttypes.arg_label = function
    | Nolabel -> Nolabel
    | Labelled s -> Labelled s.txt
    | Optional s -> Optional s.txt

  let rec expr : From.P.expression -> To.P.expression =
   fun { pexp_desc; pexp_loc; pexp_loc_stack; pexp_attributes } ->
    { pexp_desc = expr_desc pexp_desc
    ; pexp_loc = location pexp_loc
    ; pexp_loc_stack = List.map ~f:location pexp_loc_stack
    ; pexp_attributes = attributes pexp_attributes
    }

  and expr_desc : From.P.expression_desc -> To.P.expression_desc = function
    | Pexp_ident ident -> Pexp_ident (located longident ident)
    | Pexp_constant const -> Pexp_constant (constant const)
    | Pexp_apply (e, l) ->
        Pexp_apply
          (expr e, List.map l ~f:(fun (label, value) -> (arg_label label, expr value)))
    | Pexp_function (params, constr, body) ->
        Pexp_function
          ( List.map params ~f:(fun { pparam_loc; pparam_desc } : To.P.function_param ->
                { pparam_loc = location pparam_loc
                ; pparam_desc =
                    (match pparam_desc with
                    | Pparam_val (label, default, p) ->
                        Pparam_val (arg_label label, Option.map ~f:expr default, pat p)
                    | _ -> raise Stdlib.Exit)
                })
          , Option.map constr ~f:(fun _ -> raise Stdlib.Exit)
          , match body with
            | Pfunction_cases (l, loc, attrs) ->
                Pfunction_cases (cases l, location loc, attributes attrs)
            | Pfunction_body body -> Pfunction_body (expr body) )
    | Pexp_construct (id, o) -> Pexp_construct (located longident id, Option.map o ~f:expr)
    | Pexp_record (fields, base) ->
        Pexp_record
          ( List.map fields ~f:(fun (field, type_constr, value_opt) ->
                assert (Option.is_none type_constr);
                let value = Option.value_exn value_opt in
                (located longident field, expr value))
          , Option.map base ~f:expr )
    | Pexp_tuple l -> Pexp_tuple (List.map l ~f:expr)
    | Pexp_variant (s, e) -> Pexp_variant (s.txt.txt, Option.map e ~f:expr)
    | Pexp_sequence (e1, e2) -> Pexp_sequence (expr e1, expr e2)
    | Pexp_let (vbs, e, _) ->
        let rec_flag, vbs = value_bindings vbs in
        Pexp_let (rec_flag, vbs, expr e)
    | Pexp_letopen (decl, e) -> Pexp_open (open_infos ~f:module_expr decl, expr e)
    | Pexp_open _ -> assert false
    | Pexp_coerce (e, ty_opt, ty) -> Pexp_coerce (expr e, Option.map ty_opt ~f:typ, typ ty)
    | Pexp_letmodule (name, params, me, e) ->
        assert (List.is_empty params);
        Pexp_letmodule (located Fn.id name, module_expr me, expr e)
    | Pexp_letop { let_; ands; body; loc_in = _ } ->
        let binding_op : From.P.binding_op -> To.P.binding_op =
         fun { pbop_op
             ; pbop_pat
             ; pbop_exp
             ; pbop_loc
             ; pbop_args
             ; pbop_is_pun = _
             ; pbop_typ
             } ->
          assert (List.is_empty pbop_args);
          assert (Option.is_none pbop_typ);
          { pbop_op = located Fn.id pbop_op
          ; pbop_pat = pat pbop_pat
          ; pbop_exp = expr pbop_exp
          ; pbop_loc = location pbop_loc
          }
        in
        Pexp_letop
          { let_ = binding_op let_; ands = List.map ands ~f:binding_op; body = expr body }
    | Pexp_ifthenelse ([ { if_cond; if_body; if_attrs = _; if_loc_then = _ } ], e3) ->
        Pexp_ifthenelse
          (expr if_cond, expr if_body, Option.map e3 ~f:(fun (e, _) -> expr e))
    | Pexp_ifthenelse _ -> assert false
    | Pexp_while (e1, e2) -> Pexp_while (expr e1, expr e2)
    | Pexp_try (e, l) -> Pexp_try (expr e, cases l)
    | Pexp_match (e, l) -> Pexp_match (expr e, cases l)
    | Pexp_extension (name, p) -> Pexp_extension (located Fn.id name, payload p)
    | Pexp_unreachable -> Pexp_unreachable
    | Pexp_constraint (e, ty) -> Pexp_constraint (expr e, typ ty)
    | Pexp_field (e, f) -> Pexp_field (expr e, located longident f)
    | Pexp_lazy e -> Pexp_lazy (expr e)
    | Pexp_array l -> Pexp_array (List.map l ~f:expr)
    | Pexp_pack (me, ty) ->
        assert (Option.is_none ty);
        Pexp_pack (module_expr me)
    | Pexp_hole | Pexp_setfield _ | Pexp_for _ | Pexp_send _ | Pexp_new _
    | Pexp_setinstvar _ | Pexp_override _ | Pexp_letexception _ | Pexp_assert _
    | Pexp_object _ ->
        raise Stdlib.Exit
    | Pexp_list _ | Pexp_beginend _ | Pexp_parens _ | Pexp_cons _ | Pexp_indexop_access _
    | Pexp_prefix _ | Pexp_infix _ ->
        assert false

  and cases : From.P.case list -> To.P.case list = fun l -> List.map l ~f:case

  and case : From.P.case -> To.P.case =
   fun { pc_lhs; pc_guard; pc_rhs } ->
    { pc_lhs = pat pc_lhs; pc_guard = Option.map ~f:expr pc_guard; pc_rhs = expr pc_rhs }

  and typ : From.P.core_type -> To.P.core_type =
   fun { ptyp_desc; ptyp_loc; ptyp_loc_stack; ptyp_attributes } ->
    { ptyp_desc = typ_desc ptyp_desc
    ; ptyp_loc = location ptyp_loc
    ; ptyp_loc_stack = List.map ptyp_loc_stack ~f:location
    ; ptyp_attributes = attributes ptyp_attributes
    }

  and typ_desc : From.P.core_type_desc -> To.P.core_type_desc = function
    | Ptyp_any -> Ptyp_any
    | Ptyp_var v -> Ptyp_var v
    | Ptyp_arrow (params, ty2) ->
        let ty =
          List.fold_right ~init:(typ ty2) params
            ~f:(fun { pap_label; pap_loc; pap_type } ty2 ->
              To.Ast_helper.Typ.arrow ~loc:(location pap_loc) (arg_label pap_label)
                (typ pap_type) ty2)
        in
        ty.ptyp_desc
    | Ptyp_constr (id, params) ->
        Ptyp_constr (located longident id, List.map params ~f:typ)
    | Ptyp_tuple l -> Ptyp_tuple (List.map l ~f:typ)
    | Ptyp_object _ | Ptyp_class _ | Ptyp_alias _ | Ptyp_variant _ | Ptyp_poly _
    | Ptyp_package _ | Ptyp_open _ | Ptyp_extension _ ->
        raise Stdlib.Not_found

  and open_infos : type a b. a From.P.open_infos -> f:(a -> b) -> b To.P.open_infos =
   fun { popen_expr; popen_override; popen_loc; popen_attributes } ~f ->
    { popen_expr = f popen_expr
    ; popen_override = (match popen_override with Override -> Override | Fresh -> Fresh)
    ; popen_loc = location popen_loc
    ; popen_attributes = ext_attributes popen_attributes
    }

  and module_expr : From.P.module_expr -> To.P.module_expr =
   fun { pmod_desc; pmod_loc; pmod_attributes } ->
    { pmod_desc = module_expr_desc pmod_desc
    ; pmod_loc = location pmod_loc
    ; pmod_attributes = attributes pmod_attributes
    }

  and module_expr_desc : From.P.module_expr_desc -> To.P.module_expr_desc = function
    | Pmod_ident i -> Pmod_ident (located longident i)
    | Pmod_structure l -> Pmod_structure (List.map ~f:structure_item l)
    | Pmod_hole | Pmod_functor _ | Pmod_apply _ | Pmod_apply_unit _ | Pmod_constraint _
    | Pmod_unpack _ | Pmod_extension _ ->
        raise Stdlib.Exit

  and value_bindings :
      From.P.value_bindings -> To.Asttypes.rec_flag * To.P.value_binding list =
   fun { pvbs_bindings; pvbs_rec } ->
    (rec_flag pvbs_rec, List.map pvbs_bindings ~f:value_binding)

  and value_binding : From.P.value_binding -> To.P.value_binding =
   fun { pvb_pat
       ; pvb_body
       ; pvb_constraint
       ; pvb_attributes
       ; pvb_loc
       ; pvb_args = _
       ; pvb_is_pun = _
       } ->
    { pvb_pat = pat pvb_pat
    ; pvb_expr =
        (match pvb_body with
        | Pfunction_cases _ -> assert false
        | Pfunction_body body -> expr body)
    ; pvb_constraint = Option.map pvb_constraint ~f:(fun _ -> raise Stdlib.Exit)
    ; pvb_attributes = ext_attributes pvb_attributes
    ; pvb_loc = location pvb_loc
    }

  and rec_flag : From.Asttypes.rec_flag -> To.Asttypes.rec_flag = function
    | Recursive -> Recursive
    | Nonrecursive -> Nonrecursive

  and constant : From.P.constant -> To.P.constant =
   fun { pconst_desc; pconst_loc } ->
    { pconst_desc = constant_desc pconst_desc; pconst_loc = location pconst_loc }

  and constant_desc : From.P.constant_desc -> To.P.constant_desc = function
    | Pconst_char (c, _) -> Pconst_char c
    | Pconst_integer (s, c) -> Pconst_integer (s, c)
    | Pconst_string (s, loc, delim) -> Pconst_string (s, location loc, delim)
    | Pconst_float (s, c) -> Pconst_float (s, c)

  and pat : From.P.pattern -> To.P.pattern =
   fun { ppat_desc; ppat_loc; ppat_attributes; ppat_loc_stack } ->
    { ppat_desc = pat_desc ppat_desc
    ; ppat_loc = location ppat_loc
    ; ppat_loc_stack = List.map ~f:location ppat_loc_stack
    ; ppat_attributes = attributes ppat_attributes
    }

  and pat_desc : From.P.pattern_desc -> To.P.pattern_desc = function
    | Ppat_any -> Ppat_any
    | Ppat_var v -> Ppat_var (located Fn.id v)
    | Ppat_construct (id, o) ->
        Ppat_construct
          ( located longident id
          , Option.map o ~f:(fun (types, p) -> (List.map types ~f:(located Fn.id), pat p))
          )
    | Ppat_variant (s, e) -> Ppat_variant (s.txt.txt, Option.map e ~f:pat)
    | Ppat_tuple l -> Ppat_tuple (List.map l ~f:pat)
    | Ppat_constraint (p, ty) -> Ppat_constraint (pat p, typ ty)
    | Ppat_extension (name, p) -> Ppat_extension (located Fn.id name, payload p)
    | Ppat_alias (p, name) -> Ppat_alias (pat p, located Fn.id name)
    | Ppat_constant const -> Ppat_constant (constant const)
    | Ppat_interval (c1, c2) -> Ppat_interval (constant c1, constant c2)
    | Ppat_record (fields, closed) ->
        Ppat_record
          ( List.map fields ~f:(fun (field, ty, p) ->
                assert (Option.is_none ty);
                let p = Option.value_exn p in
                (located longident field, pat p))
          , match closed with OClosed -> Closed | OOpen _ -> Open )
    | Ppat_or [] -> assert false
    | Ppat_or (p :: ps) ->
        (List.fold_left ps ~init:(pat p) ~f:(fun acc p ->
             To.Ast_helper.Pat.or_ acc (pat p)))
          .ppat_desc
    | Ppat_lazy p -> Ppat_lazy (pat p)
    | Ppat_array _ | Ppat_type _ | Ppat_unpack _ | Ppat_exception _ | Ppat_effect _
    | Ppat_open _ ->
        raise Stdlib.Exit
    | Ppat_cons _ | Ppat_list _ -> assert false

  and attributes : From.P.attributes -> To.P.attributes = fun l -> List.map l ~f:attribute

  and ext_attributes : From.P.ext_attrs -> To.P.attributes =
   fun { attrs_before; attrs_after; attrs_extension } ->
    if Option.is_some attrs_extension then raise Stdlib.Exit;
    attributes attrs_before @ attributes attrs_after

  and attribute : From.P.attribute -> To.P.attribute =
   fun { attr_name; attr_payload; attr_loc } ->
    { attr_name = located Fn.id attr_name
    ; attr_payload = payload attr_payload
    ; attr_loc = location attr_loc
    }

  and payload : From.P.payload -> To.P.payload = function
    | PStr l -> PStr (structure l)
    | PSig l -> PSig (signature l)
    | PPat (p, e) -> PPat (pat p, Option.map e ~f:expr)
    | PTyp t -> PTyp (typ t)

  and structure : From.P.structure -> To.P.structure =
   fun l -> List.map l ~f:structure_item

  and signature : From.P.signature -> To.P.signature =
   fun l -> List.map l ~f:signature_item

  and signature_item : From.P.signature_item -> To.P.signature_item =
   fun _ -> raise Stdlib.Exit

  and structure_item : From.P.structure_item -> To.P.structure_item =
   fun { pstr_desc; pstr_loc } ->
    { pstr_desc = structure_item_desc pstr_desc; pstr_loc = location pstr_loc }

  and structure_item_desc : From.P.structure_item_desc -> To.P.structure_item_desc =
    function
    | Pstr_eval (e, attrs) -> Pstr_eval (expr e, attributes attrs)
    | Pstr_value vbs ->
        let rec_flag, vbs = value_bindings vbs in
        Pstr_value (rec_flag, vbs)
    | Pstr_primitive _ | Pstr_type _ | Pstr_typext _ | Pstr_exception _ | Pstr_module _
    | Pstr_recmodule _ | Pstr_modtype _ | Pstr_open _ | Pstr_class _ | Pstr_class_type _
    | Pstr_include _ | Pstr_attribute _ | Pstr_extension _ ->
        raise Stdlib.Exit
end
