open Base
open Stdio
open! Common

let run ~type_index file_path =
  let file_contents = In_channel.read_all (Cwdpath.to_string file_path) in
  Load_path.init ~auto_include:Load_path.no_auto_include
    ~visible:[ Config.standard_library ] ~hidden:[];
  let impl =
    let lexbuf = Lexing.from_string file_contents in
    Lexing.set_filename lexbuf (Cwdpath.to_string file_path);
    Uast.Parse.implementation lexbuf
  in
  let super = Ast_mapper.default_mapper in
  let unit_type =
    Uast.type_type
      (Ast_helper.Typ.constr { txt = Lident "unit"; loc = !Ast_helper.default_loc } [])
  in
  let matches = Queue.create () in
  let self =
    { super with
      expr =
        (fun self expr ->
          (match expr.pexp_desc with
          | Pexp_sequence (e1, _) when not e1.pexp_loc.loc_ghost -> (
              (* Maybe we could iterate over the typedtree directly? We'd have to pray
                 that any ppx sets loc_ghost on all the generated code. This seems more
                 robust. *)
              match Build.Type_index.exp type_index e1.pexp_loc with
              | [] | _ :: _ :: _ ->
                  (* there can be no type information, in attributes for instance *)
                  ()
              | [ texpr ] ->
                  if
                    Uast.match_typ ~env:(force Uast.initial_env) texpr.exp_type
                      ~user_type:unit_type
                  then ()
                  else Queue.enqueue matches e1.pexp_loc)
          | _ -> ());
          super.expr self expr)
    }
  in
  ignore (self.structure self impl : Parsetree.structure);
  if Queue.is_empty matches
  then None
  else
    let buf = Buffer.create (String.length file_contents) in
    let pos = ref 0 in
    let copy_orig to_ =
      Buffer.add_substring buf file_contents ~pos:!pos ~len:(to_ - !pos);
      pos := to_
    in
    List.iter
      (List.sort (Queue.to_list matches) ~compare:(fun loc1 loc2 ->
           Int.compare loc1.loc_start.pos_cnum loc2.loc_start.pos_cnum))
      ~f:(fun loc ->
        copy_orig loc.loc_start.pos_cnum;
        Buffer.add_string buf "ignore (";
        copy_orig loc.loc_end.pos_cnum;
        Buffer.add_string buf ")");
    copy_orig (String.length file_contents);
    Some (file_contents, Lazy.from_val (Buffer.contents buf), None)
