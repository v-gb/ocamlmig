open Base
open Stdio
open Common

module Cst_comb = struct
  module C = Parsexp.Cst

  let rec is_leading_atom (csts : C.t_or_comment list) name =
    match csts with
    | Comment _ :: rest -> is_leading_atom rest name
    | Sexp (Atom atom) :: rest ->
        if atom.atom =: name then Some (atom.loc, rest) else None
    | Sexp (List _) :: _ | [] -> None

  let is_constructor (cst : C.t_or_comment) name =
    match cst with
    | Comment _ -> None
    | Sexp (Atom _) -> None
    | Sexp (List { loc = _; elements }) -> is_leading_atom elements name

  let rec is_constructor' (csts : C.t_or_comment list) name =
    match csts with
    | [] -> None
    | Comment _ :: rest -> is_constructor' rest name
    | (Sexp _ as cst) :: _ -> is_constructor cst name

  let rec is_field (csts : C.t_or_comment list) name =
    match csts with
    | [] -> None
    | (Comment _ | Sexp (Atom _)) :: rest -> is_field rest name
    | Sexp (List list) :: rest -> (
        match is_leading_atom list.elements name with
        | Some (atom_loc, rest) -> Some (atom_loc, list.loc, rest)
        | None -> is_field rest name)

  let rec leading_atom (csts : C.t_or_comment list) =
    match csts with
    | Comment _ :: rest -> leading_atom rest
    | Sexp (Atom { loc; atom; unescaped }) :: rest -> Some ((loc, atom, unescaped), rest)
    | Sexp (List _) :: _ | [] -> None

  let rec atom_or_leading_atom (csts : C.t_or_comment list) =
    match csts with
    | [] -> None
    | Comment _ :: rest -> atom_or_leading_atom rest
    | Sexp (Atom atom) :: rest -> Some (atom.atom, atom.loc, rest)
    | Sexp (List list) :: rest -> (
        match leading_atom list.elements with
        | None -> None
        | Some ((_, atom, _), _) -> Some (atom, list.loc, rest))

  let rec is_atom_or_leading_atom (csts : C.t_or_comment list) name =
    match csts with
    | [] -> None
    | Comment _ :: rest -> is_atom_or_leading_atom rest name
    | Sexp (Atom atom) :: rest ->
        if atom.atom =: name
        then Some (`Atom atom.loc)
        else is_atom_or_leading_atom rest name
    | Sexp (List list) :: rest -> (
        match is_leading_atom list.elements name with
        | None -> is_atom_or_leading_atom rest name
        | Some (atom_loc, atom_tail) -> Some (`List (atom_loc, list.loc, atom_tail)))

  let all_atoms_or_leading_atoms (csts : C.t_or_comment list) =
    let rec loop acc rest =
      match atom_or_leading_atom rest with
      | None -> List.rev acc
      | Some (atom, ctx, rest) -> loop ((atom, ctx) :: acc) rest
    in
    loop [] csts
end

let update_text text changes =
  if List.is_empty changes
  then None
  else
    let changes = List.stable_sort changes ~compare:[%compare: int * _] in
    let buf = Buffer.create (String.length text) in
    let pos = ref 0 in
    let flush_to pos' =
      assert (pos' >= !pos);
      Buffer.add_substring buf text ~pos:!pos ~len:(pos' - !pos);
      pos := pos'
    in
    List.iter changes ~f:(fun (pos1, delta) ->
        flush_to pos1;
        match delta with
        | `Add str -> Buffer.add_string buf str
        | `Remove pos2 ->
            assert (pos2 >= !pos);
            pos := pos2);
    flush_to (String.length text);
    Some (text, Buffer.contents buf)

type range = Parsexp.Positions.range

let add_to_list q ((field_loc : range), (_list_loc : range), fields) atoms_to_add =
  List.iter atoms_to_add ~f:(fun (atom_to_add, ~sexp:sexp_to_add) ->
      let atoms = Cst_comb.all_atoms_or_leading_atoms fields in
      if List.exists atoms ~f:(fun (atom, _) -> atom =: atom_to_add)
      then ()
      else
        let surrounding =
          With_return.with_return (fun r ->
              let next = ref None in
              List.iter (List.rev atoms) ~f:(fun (atom, loc) ->
                  if atom_to_add >: atom then r.return (Some loc, !next);
                  next := Some loc);
              (None, !next))
        in
        let insert_after (loc : range) ~same_line =
          Queue.enqueue q
            ( loc.end_pos.offset
            , `Add
                (if same_line
                 then " " ^ sexp_to_add
                 else "\n" ^ String.make loc.start_pos.col ' ' ^ sexp_to_add) )
        in
        let insert_before (loc : range) ~same_line =
          Queue.enqueue q
            ( loc.start_pos.offset
            , `Add
                (if same_line
                 then sexp_to_add ^ " "
                 else sexp_to_add ^ "\n" ^ String.make loc.start_pos.col ' ') )
        in
        match surrounding with
        | None, None -> insert_after field_loc ~same_line:true
        | Some loc1, None ->
            let same_line =
              match List.nth atoms (List.length atoms - 2) with
              | Some (_, loc1_prev) -> loc1_prev.start_pos.line = loc1.start_pos.line
              | None -> false
            in
            insert_after loc1 ~same_line
        | None, Some loc2 ->
            let same_line =
              match List.nth atoms 2 with
              | Some (_, loc2_next) -> loc2_next.start_pos.line = loc2.start_pos.line
              | None -> false
            in
            insert_before loc2 ~same_line
        | Some loc1, Some loc2 ->
            let same_line = loc1.start_pos.line = loc2.start_pos.line in
            insert_before loc2 ~same_line)

let package_of_public_name dep =
  match String.lsplit2 dep ~on:'.' with Some (package, _) -> package | None -> dep

let compare_version v1 v2 =
  (* Unclear how to get the opam compare version function. We expect regular release
     versions to show up in migrations rather than dev releases or whatever, so I think
     just a natural sort is going to be fine likely forever. *)
  let to_compare_key v =
    String.to_list v
    |> List.group ~break:(fun c1 c2 -> Bool.( <> ) (Char.is_digit c1) (Char.is_digit c2))
    |> List.map ~f:(fun s ->
        let s = String.of_list s in
        if Char.is_digit s.[0] then `Int (Int.of_string s) else `Str s)
  in
  [%compare: [ `Int of int | `Str of string ] list] (to_compare_key v1)
    (to_compare_key v2)

let max_version v1 v2 =
  match (v1, v2) with
  | None, v | v, None -> v
  | Some v1, Some v2 -> Some (if compare_version v1 v2 > 0 then v1 else v2)

let dune_file_public_name dune_cst =
  With_return.with_return_option (fun r ->
      List.iter dune_cst ~f:(fun elt ->
          Option.iter (Cst_comb.is_constructor elt "library") ~f:(fun (_, library_cst) ->
              Option.iter (Cst_comb.is_field library_cst "public_name")
                ~f:(fun (_, _, public_name_cst) ->
                  Option.iter (Cst_comb.leading_atom public_name_cst)
                    ~f:(fun ((_, atom, _), _) -> r.return atom)))))

let add_dependencies_to_dune_file dune_path deps =
  if Map.is_empty deps
  then (None, None)
  else
    let libs =
      Map.keys deps
      |> List.filter_map ~f:(function
        | `Lib v -> Some (v, ~sexp:(Sexp.to_string_mach (sexp_of_string v)))
        | `Pp _ -> None)
    in
    let pps =
      Map.keys deps
      |> List.filter_map ~f:(function
        | `Pp v -> Some (v, ~sexp:(Sexp.to_string_mach (sexp_of_string v)))
        | `Lib _ -> None)
    in
    let dune_contents = In_channel.read_all (Cwdpath.to_string dune_path) in
    let dune_cst = Parsexp.Many_cst.parse_string_exn dune_contents in
    let public_name = dune_file_public_name dune_cst in
    let changes =
      let q = Queue.create () in
      List.iter dune_cst ~f:(fun elt ->
          let library_or_executable_stanza =
            List.find_map ~f:Fn.id
              [ Cst_comb.is_constructor elt "library"
              ; Cst_comb.is_constructor elt "executable"
              ; Cst_comb.is_constructor elt "executables"
              ]
          in
          Option.iter library_or_executable_stanza ~f:(fun (_, library_cst) ->
              Option.iter (Cst_comb.is_field library_cst "libraries") ~f:(fun field ->
                  add_to_list q field libs);
              (* We'd need to do something else in the None case, so we add the preprocess
                 field when it doesn't already exist. *)
              Option.iter (Cst_comb.is_field library_cst "preprocess")
                ~f:(fun (_, _, pp_field) ->
                  Option.iter (Cst_comb.is_field pp_field "pps") ~f:(fun field ->
                      add_to_list q field pps))));
      Queue.to_list q
    in
    (update_text dune_contents changes, Some public_name)

let add_package_deps_bumping_versions q ((_, _, fields) as field) deps =
  let remaining_atoms =
    List.filter_map deps ~f:(fun (atom_to_add, min_version) ->
        match Cst_comb.is_atom_or_leading_atom fields atom_to_add with
        | None ->
            (* If the package is missing, we let add_to_list figure out the placement *)
            Some
              ( atom_to_add
              , ~sexp:(Sexp.to_string_hum
                         (match min_version with
                         | None -> [%sexp (atom_to_add : string)]
                         | Some min_version ->
                             [%sexp
                               (atom_to_add : string), (">=", (min_version : string))]))
              )
        | Some atom_or_leading_atom ->
            (* But if the package is already present, then we need to figure out how to
               merge what we want with what exists. *)
            (match min_version with
            | None -> ()
            | Some min_version -> (
                match
                  match atom_or_leading_atom with
                  | `Atom _ -> None
                  | `List (_, _, tail) -> Cst_comb.is_field tail ">="
                with
                | None ->
                    let lparen, rparen, atom_loc =
                      match atom_or_leading_atom with
                      | `Atom loc -> ("(", ")", loc)
                      | `List (atom_loc, _, _) -> ("", "", atom_loc)
                    in
                    Queue.enqueue q (atom_loc.start_pos.offset, `Add lparen);
                    Queue.enqueue q
                      ( atom_loc.end_pos.offset
                      , `Add
                          (Printf.sprintf " (>= %s)%s"
                             (Sexp.to_string_mach (sexp_of_string min_version))
                             rparen) )
                | Some (_, _, gt_tail) -> (
                    match Cst_comb.leading_atom gt_tail with
                    | None -> () (* syntax error it seems *)
                    | Some ((current_version_loc, current_version, _), _) ->
                        if compare_version min_version current_version > 0
                        then (
                          Queue.enqueue q
                            ( current_version_loc.start_pos.offset
                            , `Add (Sexp.to_string_mach (sexp_of_string min_version)) );
                          Queue.enqueue q
                            ( current_version_loc.start_pos.offset
                            , `Remove current_version_loc.end_pos.offset )))));
            None)
  in
  add_to_list q field remaining_atoms

module Lib_or_pp = struct
  type t =
    [ `Lib of string
    | `Pp of string
    ]
  [@@deriving compare, sexp_of]

  include (val Comparator.make ~compare ~sexp_of_t)
end

let add_dependencies_to_dune_project dune_project_path deps =
  if List.is_empty deps
  then None
  else
    (* It seems wrong to be adding all dependencies to all packages. I initially
       thought the right thing to do would be to add the dependencies coming from
       the library with public name foo.bar only to package foo, but currently
       the ocamlmig repo has no public name, and yet the package from the dune-project
       file is consulted by dune pkg. So long story short, I have no idea what
       dune is doing, and at least this works for dune-project with a single package. *)
    let deps =
      List.concat_map deps ~f:(fun (_public_name, deps) ->
          Map.to_alist deps
          |> List.map ~f:(fun ((`Lib name | `Pp name), min_v) ->
              (package_of_public_name name, min_v)))
      |> Map.of_alist_reduce (module String) ~f:max_version
      |> Map.to_alist
    in
    let dune_project_contents =
      In_channel.read_all (Cwdpath.to_string dune_project_path)
    in
    let dune_project_cst = Parsexp.Many_cst.parse_string_exn dune_project_contents in
    let changes =
      let q = Queue.create () in
      List.iter dune_project_cst ~f:(fun elt ->
          (* what if we need to add the field entirely?? *)
          Option.iter (Cst_comb.is_constructor elt "package") ~f:(fun (_, package_cst) ->
              Option.iter (Cst_comb.is_field package_cst "depends") ~f:(fun field ->
                  add_package_deps_bumping_versions q field deps)));
      Queue.to_list q
    in
    update_text dune_project_contents changes

let dune_path path =
  (* Normally, the dune file that configures a .ml file lives in the same directory.
     However, in the presence of the (include_subdirs ..) stanza, the dune file can
     live higher up. We don't check that the stanza is present here, we just assume
     the dune file we find should be the right one. Even if it wasn't, it wouldn't
     be a big deal, as it's not used for much. *)
  let default () =
    (* for cases that won't work, but we just let the caller fail *)
    Cwdpath.concat (Cwdpath.dirname path) "dune"
  in
  let rec loop dir count =
    let path = Cwdpath.concat dir "dune" in
    if Sys.file_exists (Cwdpath.to_string path)
    then path
    else if Build.is_dune_root (Cwdpath.to_string dir) || count > 100
    then default ()
    else loop (Cwdpath.concat dir "..") (count + 1)
  in
  loop (Cwdpath.dirname path) 0

let add_dependencies ~dune_root path_and_additions =
  let dune_paths_and_deps =
    List.concat_map path_and_additions
      ~f:(fun (path, ({ libraries; pps } : Add_deps.t)) ->
        List.map libraries ~f:(fun { name; min_version } ->
            (dune_path path, (`Lib name, min_version)))
        @ List.map pps ~f:(fun { name; min_version } ->
            (dune_path path, (`Pp name, min_version))))
    |> Map.of_alist_multi (module Cwdpath)
    |> Map.map ~f:(Map.of_alist_reduce (module Lib_or_pp) ~f:max_version)
    |> Map.to_alist
  in
  let deps_by_public_names = Queue.create () in
  let new_dunes =
    List.filter_map dune_paths_and_deps ~f:(fun (dune_path, deps) ->
        let change, public_name = add_dependencies_to_dune_file dune_path deps in
        Option.iter public_name ~f:(fun public_name ->
            Queue.enqueue deps_by_public_names (public_name, deps));
        Option.map change ~f:(fun (before, after) -> (~path:dune_path, ~before, ~after)))
  in
  let new_dune_project =
    let dune_project_path =
      Abspath.to_cwdpath (Abspath.concat dune_root "dune-project")
    in
    let new_dune_project =
      add_dependencies_to_dune_project dune_project_path
        (Queue.to_list deps_by_public_names)
    in
    Option.map new_dune_project ~f:(fun (before, after) ->
        (~path:dune_project_path, ~before, ~after))
  in
  new_dunes @ Option.to_list new_dune_project

let ppx ~path =
  let dune_cst =
    try
      In_channel.read_all (Cwdpath.to_string (dune_path path))
      |> Parsexp.Many_cst.parse_string_exn
    with e ->
      raise_s [%sexp "failed to read dune file for", ~~(path : Cwdpath.t), (e : exn)]
  in
  With_return.with_return_option (fun r ->
      List.iter dune_cst ~f:(fun elt ->
          Option.iter (Cst_comb.is_constructor elt "library") ~f:(fun (_, library_cst) ->
              Option.iter (Cst_comb.is_field library_cst "preprocess")
                ~f:(fun (_, _, preprocess_cst) ->
                  Option.iter (Cst_comb.is_constructor' preprocess_cst "pps")
                    ~f:(fun (_, list) ->
                      Cst_comb.all_atoms_or_leading_atoms list
                      |> List.map ~f:(fun (atom, _) -> atom)
                      |> List.dedup_and_sort ~compare:String.compare
                      |> r.return)))))
  |> Option.value ~default:[]
