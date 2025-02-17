module Fmast = struct
  include Ocamlformat_ocaml_common
  include Ocamlformat_parser_extended
end

let location : Location.t -> Ocamlformat_ocaml_common.Location.t =
 fun loc ->
  { loc_start = loc.loc_start; loc_end = loc.loc_end; loc_ghost = loc.loc_ghost }

let location' : Ocamlformat_ocaml_common.Location.t -> Location.t =
 fun loc ->
  { loc_start = loc.loc_start; loc_end = loc.loc_end; loc_ghost = loc.loc_ghost }

let located conv_a : _ Location.loc -> _ Ocamlformat_ocaml_common.Location.loc =
 fun { txt; loc } -> { txt = conv_a txt; loc = location loc }

let located' conv_a : _ Ocamlformat_ocaml_common.Location.loc -> _ Location.loc =
 fun { txt; loc } -> { txt = conv_a txt; loc = location' loc }

let rec longident' : Ocamlformat_ocaml_common.Longident.t -> Longident.t = function
  | Lident s -> Lident s
  | Ldot (t, s) -> Ldot (longident' t, s)
  | Lapply (t1, t2) -> Lapply (longident' t1, longident' t2)

let rec longident : Longident.t -> Ocamlformat_ocaml_common.Longident.t = function
  | Lident s -> Lident s
  | Ldot (t, s) -> Ldot (longident t, s)
  | Lapply (t1, t2) -> Lapply (longident t1, longident t2)

let arg_label : Asttypes.arg_label -> Ocamlformat_parser_extended.Asttypes.arg_label =
  function
  | Nolabel -> Nolabel
  | Labelled s -> Labelled { txt = s; loc = Fmast.Location.none }
  | Optional s -> Optional { txt = s; loc = Fmast.Location.none }
