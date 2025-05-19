open Base
module P = Parsetree

module Fmast = struct
  include Ocamlformat_ocaml_common
  include Ocamlformat_parser_extended
  module P = Parsetree
end

let location : Location.t -> Fmast.Location.t =
 fun loc ->
  { loc_start = loc.loc_start; loc_end = loc.loc_end; loc_ghost = loc.loc_ghost }

let location' : Fmast.Location.t -> Location.t =
 fun loc ->
  { loc_start = loc.loc_start; loc_end = loc.loc_end; loc_ghost = loc.loc_ghost }

let located conv_a : _ Location.loc -> _ Fmast.Location.loc =
 fun { txt; loc } -> { txt = conv_a txt; loc = location loc }

let located' conv_a : _ Fmast.Location.loc -> _ Location.loc =
 fun { txt; loc } -> { txt = conv_a txt; loc = location' loc }

let rec longident' : Fmast.Longident.t -> Longident.t = function
  | Lident s -> Lident s
  | Ldot (t, s) -> Ldot (longident' t, s)
  | Lapply (t1, t2) -> Lapply (longident' t1, longident' t2)

let rec longident : Longident.t -> Fmast.Longident.t = function
  | Lident s -> Lident s
  | Ldot (t, s) -> Ldot (longident t, s)
  | Lapply (t1, t2) -> Lapply (longident t1, longident t2)

let arg_label : Asttypes.arg_label -> Fmast.Asttypes.arg_label = function
  | Nolabel -> Nolabel
  | Labelled s -> Labelled { txt = s; loc = Fmast.Location.none }
  | Optional s -> Optional { txt = s; loc = Fmast.Location.none }

let rec expr : P.expression -> Fmast.P.expression =
 fun { pexp_desc; pexp_loc; pexp_loc_stack; pexp_attributes } ->
  { pexp_desc = expr_desc pexp_desc
  ; pexp_loc = location pexp_loc
  ; pexp_loc_stack = List.map ~f:location pexp_loc_stack
  ; pexp_attributes = attributes pexp_attributes
  }

and expr_desc : P.expression_desc -> Fmast.P.expression_desc = function
  | Pexp_ident ident -> Pexp_ident (located longident ident)
  | _ -> raise Stdlib.Exit

and attributes : P.attributes -> Fmast.P.attributes = fun l -> List.map l ~f:attribute

and attribute : P.attribute -> Fmast.P.attribute =
 fun { attr_name; attr_payload; attr_loc } ->
  { attr_name = located Fn.id attr_name
  ; attr_payload = payload attr_payload
  ; attr_loc = location attr_loc
  }

and payload : P.payload -> Fmast.P.payload = function
  | PStr l -> PStr (structure l)
  | _ -> raise Stdlib.Exit

and structure : P.structure -> Fmast.P.structure = fun l -> List.map l ~f:structure_item

and structure_item : P.structure_item -> Fmast.P.structure_item =
 fun { pstr_desc; pstr_loc } ->
  { pstr_desc = structure_item_desc pstr_desc; pstr_loc = location pstr_loc }

and structure_item_desc : P.structure_item_desc -> Fmast.P.structure_item_desc = function
  | Pstr_eval (e, attrs) -> Pstr_eval (expr e, attributes attrs)
  | _ -> raise Stdlib.Exit

let rec expr' : Fmast.P.expression -> P.expression =
 fun { pexp_desc; pexp_loc; pexp_loc_stack; pexp_attributes } ->
  { pexp_desc = expr_desc' pexp_desc
  ; pexp_loc = location' pexp_loc
  ; pexp_loc_stack = List.map ~f:location' pexp_loc_stack
  ; pexp_attributes = attributes' pexp_attributes
  }

and expr_desc' : Fmast.P.expression_desc -> P.expression_desc = function
  | Pexp_ident ident -> Pexp_ident (located' longident' ident)
  | _ -> raise Stdlib.Exit

and attributes' : Fmast.P.attributes -> P.attributes = fun l -> List.map l ~f:attribute'

and attribute' : Fmast.P.attribute -> P.attribute =
 fun { attr_name; attr_payload; attr_loc } ->
  { attr_name = located' Fn.id attr_name
  ; attr_payload = payload' attr_payload
  ; attr_loc = location' attr_loc
  }

and payload' : Fmast.P.payload -> P.payload = function
  | PStr l -> PStr (structure' l)
  | _ -> raise Stdlib.Exit

and structure' : Fmast.P.structure -> P.structure = fun l -> List.map l ~f:structure_item'

and structure_item' : Fmast.P.structure_item -> P.structure_item =
 fun { pstr_desc; pstr_loc } ->
  { pstr_desc = structure_item_desc' pstr_desc; pstr_loc = location' pstr_loc }

and structure_item_desc' : Fmast.P.structure_item_desc -> P.structure_item_desc = function
  | Pstr_eval (e, attrs) -> Pstr_eval (expr' e, attributes' attrs)
  | _ -> raise Stdlib.Exit
