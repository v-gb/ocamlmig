(**************************************************************************)
(*                                                                        *)
(*                              OCamlFormat                               *)
(*                                                                        *)
(*            Copyright (c) Facebook, Inc. and its affiliates.            *)
(*                                                                        *)
(*      This source code is licensed under the MIT license found in       *)
(*      the LICENSE file in the root directory of this source tree.       *)
(*                                                                        *)
(**************************************************************************)

module Location = Migrate_ast.Location
open Extended_ast
include Non_overlapping_interval_tree.Make (Location)

(** Use Ast_mapper to collect all locs in ast, and create tree of them. *)
let of_ast fragment ast =
  let attribute (m : Ast_mapper.mapper) attr =
    (* ignore location of docstrings *)
    if Ast.Attr.is_doc attr then attr
    else Ast_mapper.default_mapper.attribute m attr
  in
  let locs = ref [] in
  let location _ (loc : Location.t) =
    (* Migrated code has very early locations, so they mostly come before the
       ast, and get assigned early comments. We could shift those location
       arbitrarily, but without trying, it seems like it might lead to the
       reverse problem of final comments being assigned to random migrated
       code. Taking out these locations entirely seems more robust. *)
    if String.is_prefix loc.loc_start.pos_fname ~prefix:"_migrate" then ()
    else locs := loc :: !locs ;
    loc
  in
  (* Ignore locations of arg_labels *)
  let arg_label _ lbl = lbl in
  let mapper =
    Ast_mapper.{default_mapper with location; attribute; arg_label}
  in
  map fragment mapper ast |> ignore ;
  (of_list !locs, !locs)
