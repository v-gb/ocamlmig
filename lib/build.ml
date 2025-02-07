open Base
open! Stdio
module Filename = Stdlib.Filename
module Sys = Stdlib.Sys
module Printf = Stdlib.Printf
module Format = Stdlib.Format
open Common

let read_cmt cmt_path =
  try Cmt_format.read_cmt (Cwdpath.to_string cmt_path) with
  | Cmi_format.Error e ->
      (* This function raises both Cmi_format.Error and Cmt_format.Error. *)
      failwith (Format.asprintf "%a" Cmi_format.report_error e)
  | Cmt_format.Error e -> (
      match e with
      | Not_a_typedtree s ->
          raise_s [%sexp (cmt_path : Cwdpath.t), "not a typedtree", (s : string)])

let read_cmi cmi_path =
  try Cmi_format.read_cmi (Cwdpath.to_string cmi_path)
  with Cmi_format.Error e -> failwith (Format.asprintf "%a" Cmi_format.report_error e)

let comp_unit_of_uid (uid : Shape.Uid.t) =
  match uid with
  | Internal | Predef _ -> None
  | Compilation_unit comp_unit | Item { comp_unit; _ } -> Some comp_unit

let is_dune_root dir = Sys.file_exists (Filename.concat dir "_build")

let input_name_matching_compilation_command (cmt_infos : Cmt_format.cmt_infos) =
  (* The computed "input name" will become the pos_fname in locations in the AST we
     parse. We need it to be equal to pos_fname in the .cmt files, otherwise we will
     never line up the (ocamlformat) parsetree with the typedtree.

     Considering that the position as seen by the compiler is relative to whatever the
     build system decided (with dune, it's apparently relative to the project root),
     and that if the filesystem might be case-insensitive/preserving, or perhaps even
     do utf8 normalization, it seems best to try and get the filename from the .cmt
     rather than trying to mirror the computation done by the filesystem.

     Perhaps another solution would be not compare the filename at all, but that seems
     like that could cause problems with files that use position directives. *)
  Option.map cmt_infos.cmt_sourcefile ~f:(fun path ->
      (* The compiler sees foo.pp.ml by default with dune, due to its staging of ppxes. *)
      String.substr_replace_all path ~pattern:".pp" ~with_:"")

module Listing = struct
  type merlin_thing =
    | INDEX of string
    | STDLIB of string
    | SOURCE_ROOT of string
    | EXCLUDE_QUERY_DIR of Sexp.t list [@sexp.list]
    | B of string
    | S of string
    | FLG of string list
    | UNIT_NAME of string
  [@@deriving of_sexp]

  type one =
    { root : Abspath.t
    ; source : Cwdpath.t
    ; compilation_unit : string
    ; cmt_dirs : Cwdpath.t list
    ; cmt_load_paths : (Load_path.Dir.t Lazy.t list[@sexp.opaque])
    }
  [@@deriving sexp_of]

  type t =
    { by_source : one Map.M(Cwdpath).t
    ; by_compilation_unit : one list Map.M(String).t
    ; all_load_paths : (Load_path.Dir.t Lazy.t[@sexp.opaque]) Hashtbl.M(Cwdpath).t
    }
  [@@deriving sexp_of]

  let create_internal all_load_paths artifacts =
    { by_source =
        Map.of_alist_exn (module Cwdpath) (List.map artifacts ~f:(fun a -> (a.source, a)))
    ; by_compilation_unit =
        Map.of_alist_multi
          (module String)
          (List.map artifacts ~f:(fun a -> (a.compilation_unit, a)))
    ; all_load_paths
    }

  let cmt_load_paths_from_cmt_dirs load_path_cache cmt_dirs =
    List.map cmt_dirs ~f:(fun cmt_dir ->
        Hashtbl.find_or_add load_path_cache cmt_dir ~default:(fun () ->
            lazy (Load_path.Dir.create (Cwdpath.to_string cmt_dir) ~hidden:false)))

  let of_merlin_sexp ~load_path_cache ~dune_root source sexp =
    let merlin_items = [%of_sexp: merlin_thing list] sexp in
    let cmt_dirs =
      List.filter_map merlin_items ~f:(function
        | STDLIB dir | B dir -> Some (Cwdpath.create_abs_exn dir)
        | _ -> None)
    in
    let compilation_unit =
      List.find_map_exn merlin_items ~f:(function UNIT_NAME s -> Some s | _ -> None)
    in
    { root = dune_root
    ; source
    ; compilation_unit
    ; cmt_dirs
    ; cmt_load_paths = cmt_load_paths_from_cmt_dirs load_path_cache cmt_dirs
    }

  let dune_exe ~dune_root =
    if Sys.file_exists (Abspath.to_string (Abspath.concat dune_root "dune.lock"))
    then [ "dune" ]
    else [ "opam"; "exec"; "--"; "dune" ]

  let find_ignore_vcs dir args =
    run_process
      (List.concat
         [ [ "find"; Cwdpath.to_string dir; "(" ]
         ; List.map [ "_build"; "_opam"; ".git"; ".hg"; ".jj" ] ~f:(fun dir ->
               [ "-name"; dir ])
           |> List.intersperse ~sep:[ "-or" ]
           |> List.concat
         ; [ ")"; "-prune"; "-or" ]
         ; args
         ; [ "-printf"; "%P\\n" ]
         ])
    |> String.split_lines
    |> List.map ~f:(Cwdpath.concat dir)

  let create ~dune_root ~source_paths =
    let load_path_cache = Hashtbl.create (module Cwdpath) in
    let module Csexp = Csexp.Make (Sexp) in
    with_process_full
      (dune_exe ~dune_root @ [ "ocaml"; "merlin"; "start-session" ])
      (fun (stdout, stdin) ->
        List.filter_map source_paths ~f:(fun source_path ->
            (* Dune seems to take paths relative to wherever we started it *)
            Csexp.to_channel stdin
              (List [ Atom "File"; Atom (Cwdpath.to_string source_path) ]);
            Out_channel.flush stdin;
            match Csexp.input_opt stdout with
            | Error err -> failwith err
            | Ok None -> raise_s [%sexp "eof from dune"]
            | Ok (Some (List [ List [ Atom "ERROR"; err ] ])) ->
                eprint_s err;
                None
            | Ok (Some sexp) ->
                Some (of_merlin_sexp ~dune_root ~load_path_cache source_path sexp)))
    |> create_internal load_path_cache

  let create_without_dune root =
    let load_path_cache = Hashtbl.create (module Cwdpath) in
    let cmt_paths = find_ignore_vcs (Abspath.to_cwdpath root) [ "-name"; "*.cmt" ] in
    List.filter_map cmt_paths ~f:(fun cmt_path ->
        let cmt_infos = read_cmt cmt_path in
        match cmt_infos.cmt_sourcefile with
        | None -> None
        | Some source ->
            let cmt_dirs =
              if true
              then [ Cwdpath.dirname cmt_path ]
              else
                (* Cwdpath.create_list is probably wrong, but it's deadcode, so
                   whatever .*)
                Cwdpath.create_list
                  (cmt_infos.cmt_loadpath.visible @ cmt_infos.cmt_loadpath.hidden)
            in
            Some
              { root
              ; source =
                  Cwdpath.concat (Cwdpath.dirname cmt_path) (Filename.basename source)
              ; compilation_unit = cmt_infos.cmt_modname
              ; cmt_dirs
              ; cmt_load_paths = cmt_load_paths_from_cmt_dirs load_path_cache cmt_dirs
              })
    |> create_internal load_path_cache

  let how_to_build_cmt () =
    "Either build that part of the repository, or if you already have, try building \
     @check (dune doesn't build some .cmt files by default otherwise \
     (https://github.com/ocaml/dune/issues/3182))."

  let warn_if_cmt_seems_stale _t ~cmt_path ~source_path =
    (* The first try at a check was saying: if the .ml is changed, the .cmt will be
       rewritten (whether its contents change or not), so .ml mtime < .cmt mtime.

       But if dune uses a shared cache, it may pull an old cmt from cache by copying
       (thus fresh mtime), or hardlinking (thus fresh ctime), so .ml mtime < max(.cmt
       mtime, .cmt ctime).

       However, this logic breaks down when the .ml is updated without changing its
       contents, for instance if a .ml is generated by a build rule, or if you're
       applying sed -i on files, some of which don't change. This results in spurious
       warnings.

       So instead, we say: if the cmx is newer than than the .ml (meaning the .ml
       changed enough to be worth rebuilding), then the .cmt should be newer than the
       .ml as well. *)
    let cmx_path =
      let s = Cwdpath.to_string cmt_path in
      Filename.concat
        (Filename.concat (Filename.dirname (Filename.dirname s)) "native")
        (Filename.remove_extension (Filename.basename s) ^ ".cmx")
    in
    match Unix.lstat cmx_path with
    | exception Unix.Unix_error (ENOENT, _, _) -> ()
    | cmx_stat ->
        let seems_stale =
          let source_stat = Unix.lstat (Cwdpath.to_string source_path) in
          fun (artifact_stat : Unix.stats) ->
            Float.( > ) source_stat.st_mtime
              (Float.max artifact_stat.st_mtime artifact_stat.st_ctime)
        in
        let cmt_stat = Unix.lstat (Cwdpath.to_string cmt_path) in
        if seems_stale cmt_stat && not (seems_stale cmx_stat)
        then
          eprint_s
            [%sexp
              "Warning"
            , ( ("type information for " ^ Cwdpath.to_string source_path ^ " seems stale"
                  : string)
              , (if debug.all
                 then
                   Some
                     [ "ctime of "
                       ^ Cwdpath.to_string cmt_path
                       ^ " older than mtime of "
                       ^ Cwdpath.to_string source_path
                       ^ ")"
                     ; "while ctime of "
                       ^ cmx_path
                       ^ " NOT older than mtime of "
                       ^ Cwdpath.to_string source_path
                       ^ ")"
                     ]
                 else None
                  : (string list option[@sexp.option])) )
            , (how_to_build_cmt () : string)]

  let locate_cmt t ~source_path =
    let e () =
      Error
        [%sexp
          "no type information (.cmt) found. Is the repository fully built?"
        , `looked_in
            (if debug.all
             then Option.map (Map.find t.by_source source_path) ~f:(fun a -> a.cmt_dirs)
             else None
              : (Cwdpath.t list option[@sexp.option]))]
    in
    match Map.find t.by_source source_path with
    | None -> e ()
    | Some a -> (
        match
          List.find_map a.cmt_load_paths ~f:(fun (lazy dir) ->
              Load_path.Dir.find_normalized dir (a.compilation_unit ^ ".cmt"))
        with
        | Some cmt_path ->
            let cmt_path =
              (* This is a path relative to cmt_dirs, which is a cwdpath *)
              Cwdpath.create cmt_path
            in
            warn_if_cmt_seems_stale t ~cmt_path ~source_path;
            Ok (cmt_path, a)
        | None -> (
            match
              List.find_map a.cmt_load_paths ~f:(fun (lazy dir) ->
                  Load_path.Dir.find_normalized dir (a.compilation_unit ^ ".cmi"))
            with
            | Some _cmi_path ->
                (* In practice, this happens for the main file of executable, because such
                   .ml files are built in native code but not bytecode, and the bytecode
                   rule is the only one that builds .cmt files. *)
                Error
                  [%sexp
                    ("no type information (.cmt) found. " ^ how_to_build_cmt () : string)]
            | None -> e ()))
end

module Artifacts = struct
  module type Shape_reduce = module type of Shape_reduce.Make (struct
    let fuel = assert false
    let read_unit_shape = assert false
  end)

  type loaded_cmt =
    { path : Cwdpath.t
    ; noapprox_impl : Shape.t option Lazy.t
    ; infos : Cmt_format.cmt_infos
    ; ident_occurrences :
        Uast.Shape_reduce.result Map.M(Uast.Longident_loc_ignoring_filename).t Lazy.t
    ; m : (module Shape_reduce)
    }

  let sexp_of_loaded_cmt loaded_cmt = [%sexp { path : Cwdpath.t = loaded_cmt.path }]

  type loaded_cmi =
    { infos : Cmi_format.cmi_infos
    ; defs : Types.signature_item Shape.Uid.Tbl.t Lazy.t
    }

  let __ (x : loaded_cmi) = x.infos
  let sexp_of_loaded_cmi _loaded_cmi = [%sexp ()]

  (* To cache the loading of cmts across despite possible variations in the load
     paths. Maybe we should optimize further for the case of the load path not
     changing? It depends how big libraries are, on average. *)
  type cache =
    { impls : loaded_cmt Hashtbl.M(String).t
    ; intfs : loaded_cmt Hashtbl.M(String).t
    ; cmis : loaded_cmi Hashtbl.M(String).t
    }
  [@@deriving sexp_of]

  let create_cache () =
    { impls = Hashtbl.create (module String)
    ; intfs = Hashtbl.create (module String)
    ; cmis = Hashtbl.create (module String)
    }

  type t =
    { cache : cache option
    ; impls : (Cwdpath.t * loaded_cmt) option Hashtbl.M(String).t
    ; intfs : (Cwdpath.t * loaded_cmt) option Hashtbl.M(String).t
    ; cmis : (Cwdpath.t * loaded_cmi) option Hashtbl.M(String).t
    ; load_path_dirs : (Load_path.Dir.t list[@sexp.opaque])
    }
  [@@deriving sexp_of]

  let create ?cache (listing : Listing.t) =
    let load_path_dirs =
      Hashtbl.to_alist listing.all_load_paths
      |> List.sort ~compare:(fun (d1, _) (d2, _) -> Cwdpath.compare d1 d2)
      |> List.map ~f:(fun (_, (lazy load_path)) -> load_path)
    in
    { cache
    ; load_path_dirs
    ; impls = Hashtbl.create (module String)
    ; intfs = Hashtbl.create (module String)
    ; cmis = Hashtbl.create (module String)
    }

  let approx_id = 1_000_000_000

  (* When faced with things like:

        {[
           module M = (val ...)
           let foo = M.foo
        ]}

     the compiler gives M the shape { desc = Leaf; uid = X }, and M.foo the shape
     { desc = Leaf; approximated = true; uid = X } (in Shape.proj). I guess this is
     behavior that merlin needs, baked into the compiler. For our purpose, we want either
     the real definition (to look for [@@migrate]), or a normal form (if someone created a
     side migration with let _ = foo [@migrate ...] and we're now looking at whether some
     use of foo has a side migration). The only thing we can do with approximate data is
     ignore it, but it means we can't even attach migrations to any such identifier. It's
     preferable for us to transform all approximated nodes into unique leaves, because
     even though we won't have the real definition either way, at least we'll be able to
     compute a normal form. *)
  let no_approximated_shapes_thank_you ~comp_unit shape =
    let ids = ref 0 in
    let cache = Shape.Uid.Tbl.create 12 in
    let rec loop_t ({ uid; desc; approximated } as shape2 : Shape.t) : Shape.t =
      if approximated
      then
        { uid =
            (if approximated
             then
               let uid =
                 match uid with
                 | Some x -> x
                 | None -> Shape.Uid.mk ~current_unit:comp_unit
               in
               match uid with
               | Item { comp_unit = _; id } as foo ->
                   let module Obj = Stdlib.Obj in
                   let field_idx = 1 in
                   assert (id = Obj.obj (Obj.field (Obj.repr foo) field_idx));
                   ids := !ids + 1;
                   let foo2 = Sys.opaque_identity (Obj.dup (Obj.repr foo)) in
                   Obj.set_field (Obj.repr foo2) field_idx (Obj.repr (approx_id + !ids));
                   Some (Sys.opaque_identity (Obj.obj foo2 : Shape.Uid.t))
               | _ ->
                   raise_s
                     [%sexp
                       "unexpected approximated shape"
                     , (shape2 : Uast.Shape.t)
                     , (shape : Uast.Shape.t)]
             else uid)
        ; desc = Leaf
        ; approximated = false
        }
      else
        match
          match uid with
          | None | Some Internal (* probably not a good key *) -> None
          | Some uid -> Shape.Uid.Tbl.find_opt cache uid
        with
        | Some t -> t
        | None ->
            let t : Shape.t = { uid; desc = loop_desc desc; approximated } in
            Option.iter uid ~f:(fun uid -> Shape.Uid.Tbl.replace cache uid t);
            t
    and loop_desc = function
      | (Var _ | Leaf | Comp_unit _ | Error _) as desc -> desc
      | Abs (var, t) -> Abs (var, loop_t t)
      | App (t1, t2) -> App (loop_t t1, loop_t t2)
      | Struct map -> Struct (Shape.Item.Map.map loop_t map)
      | Alias t -> Alias (loop_t t)
      | Proj (t, item) -> Proj (loop_t t, item)
    in
    loop_t shape

  let rec create_loaded_cmt t which ~comp_unit ~(cmt_path : Cwdpath.t) : loaded_cmt =
    match t.cache with
    | None -> create_loaded_cmt_uncached t ~comp_unit ~cmt_path
    | Some cache ->
        Hashtbl.find_or_add
          (match which with `impl -> cache.impls | `intf -> cache.intfs)
          comp_unit
          ~default:(fun () -> create_loaded_cmt_uncached t ~comp_unit ~cmt_path)

  and create_loaded_cmt_uncached t ~comp_unit ~(cmt_path : Cwdpath.t) =
    let cmt_infos = read_cmt cmt_path in
    if comp_unit <>: cmt_infos.cmt_modname
    then
      raise_s
        [%sexp
          "invariant failure"
        , ~~(comp_unit : string)
        , "<>"
        , ~~(cmt_infos.cmt_modname : string)];
    let module M = struct
      include Shape_reduce.Make (struct
        let fuel = 20

        let read_unit_shape ~unit_name =
          match loaded_cmt `impl t ~comp_unit:unit_name with
          | None -> None
          | Some (_, loaded_cmt) -> force loaded_cmt.noapprox_impl
      end)
    end in
    { path = cmt_path
    ; noapprox_impl =
        lazy
          (Option.map cmt_infos.cmt_impl_shape
             ~f:(no_approximated_shapes_thank_you ~comp_unit))
    ; infos = cmt_infos
    ; ident_occurrences =
        lazy
          (cmt_infos.cmt_ident_occurrences
          |> Map.of_alist_multi (module Uast.Longident_loc_ignoring_filename)
          |> Map.map ~f:List.hd_exn)
    ; m = (module M)
    }

  and loaded_cmt which t ~comp_unit : (Cwdpath.t * _) option =
    Hashtbl.find_or_add
      (match which with `impl -> t.impls | `intf -> t.intfs)
      comp_unit
      ~default:(fun () ->
        match
          List.find_map t.load_path_dirs ~f:(fun dir ->
              Load_path.Dir.find_normalized dir
                (comp_unit ^ match which with `impl -> ".cmt" | `intf -> ".cmti"))
        with
        | None -> None
        | Some cmt_path ->
            let cmt_path =
              (* This is a path relative to cmt_dirs, which is a cwdpath *)
              Cwdpath.create cmt_path
            in
            Some (cmt_path, create_loaded_cmt t which ~comp_unit ~cmt_path))

  let index_declarations (items : Types.signature_item list) =
    let index : Types.signature_item Shape.Uid.Tbl.t = Types.Uid.Tbl.create 16 in
    let super = Btype.type_iterators in
    let self =
      { super with
        it_signature_item =
          (fun self signature_item ->
            Types.Uid.Tbl.add index
              (match signature_item with
              | Sig_value (_, d, _) -> d.val_uid
              | Sig_type (_, d, _, _) -> d.type_uid
              | Sig_typext (_, d, _, _) -> d.ext_uid
              | Sig_module (_, _, d, _, _) -> d.md_uid
              | Sig_modtype (_, d, _) -> d.mtd_uid
              | Sig_class (_, d, _, _) -> d.cty_uid
              | Sig_class_type (_, d, _, _) -> d.clty_uid)
              signature_item;
            super.it_signature_item self signature_item)
      ; it_type_expr = (fun _ _ -> () (* maybe faster *))
      }
    in
    List.iter items ~f:(self.it_signature_item self);
    List.iter items ~f:(Btype.unmark_iterators.it_signature_item Btype.unmark_iterators);
    index

  let create_loaded_cmi_uncached ~comp_unit ~(cmi_path : Cwdpath.t) =
    let cmi_infos = read_cmi cmi_path in
    if comp_unit <>: cmi_infos.cmi_name
    then
      raise_s
        [%sexp
          "invariant failure"
        , ~~(comp_unit : string)
        , "<>"
        , ~~(cmi_infos.cmi_name : string)];
    { infos = cmi_infos; defs = lazy (index_declarations cmi_infos.cmi_sign) }

  let create_loaded_cmi t ~comp_unit ~(cmi_path : Cwdpath.t) : loaded_cmi =
    match t.cache with
    | None -> create_loaded_cmi_uncached ~comp_unit ~cmi_path
    | Some cache ->
        Hashtbl.find_or_add cache.cmis comp_unit ~default:(fun () ->
            create_loaded_cmi_uncached ~comp_unit ~cmi_path)

  let loaded_cmi t ~comp_unit =
    Hashtbl.find_or_add t.cmis comp_unit ~default:(fun () ->
        match
          List.find_map t.load_path_dirs ~f:(fun dir ->
              Load_path.Dir.find_normalized dir (comp_unit ^ ".cmi"))
        with
        | None -> None
        | Some cmi_path ->
            let cmi_path =
              (* This is a path relative to cmt_dirs, which is a cwdpath *)
              Cwdpath.create cmi_path
            in
            Some (cmi_path, create_loaded_cmi t ~comp_unit ~cmi_path))

  let parse_library_name library_name =
    let library_dir = String.tr library_name ~target:'.' ~replacement:'/' in
    let base_library_name =
      Unit_info.normalize (List.last_exn (String.split library_name ~on:'.'))
    in
    let comp_unit = Unit_info.modulize base_library_name in
    (`library_dir library_dir, `base_library_name base_library_name, `comp_unit comp_unit)

  let locate_cmt_from_library_name_in_opam t
      ( `library_dir library_dir
      , `base_library_name base_library_name
      , `comp_unit comp_unit ) =
    List.find_map t.load_path_dirs ~f:(fun dir ->
        Load_path.Dir.find_normalized dir "stdlib.cmi")
    |> Option.bind ~f:(fun stdlib_cmi ->
           let libdir = Filename.dirname (Filename.dirname stdlib_cmi) in
           Load_path.Dir.find_normalized
             (Load_path.Dir.create (Filename.concat libdir library_dir) ~hidden:false)
             (base_library_name ^ ".cmt")
           |> Option.map ~f:(fun cmt_path ->
                  let cmt_path =
                    (* This is a path relative to cmt_dirs, which is a cwdpath *)
                    Cwdpath.create cmt_path
                  in
                  (cmt_path, create_loaded_cmt t `impl ~comp_unit ~cmt_path)))

  let locate_cmt_from_library_name t ~dune_root ~library_name =
    (* The side migration code will look for an entry in this cache. Maybe we should
       entangle this. *)
    let ((`library_dir _, `base_library_name base_library_name, `comp_unit comp_unit) as
         parsed_library_name) =
      parse_library_name library_name
    in
    Hashtbl.find_or_add t.impls comp_unit ~default:(fun () ->
        if library_name <>: base_library_name
        then locate_cmt_from_library_name_in_opam t parsed_library_name
        else
          let start_dir = Abspath.concat (Abspath.concat dune_root "_build") "default" in
          match
            Listing.find_ignore_vcs
              (Abspath.to_cwdpath start_dir)
              [ "-name"; library_name ^ ".cmt" ]
          with
          | _ :: _ :: _ as cmt_paths ->
              raise_s
                [%sexp
                  "multiple possible .cmt files for"
                , ~~(library_name : string)
                , ~~(cmt_paths : Cwdpath.t list)]
          | [ cmt_path ] -> Some (cmt_path, create_loaded_cmt t `impl ~comp_unit ~cmt_path)
          | [] -> locate_cmt_from_library_name_in_opam t parsed_library_name)
    |> Option.map ~f:(fun (cmt_path, loaded_cmt) -> (cmt_path, loaded_cmt.infos))

  let decl_from_def_uid t (uid, impl_or_intf) =
    Option.bind (comp_unit_of_uid uid) ~f:(fun comp_unit ->
        match
          match (loaded_cmt impl_or_intf t ~comp_unit, impl_or_intf) with
          | None, `intf -> loaded_cmt `impl t ~comp_unit
          | res, _ -> res
        with
        | None -> None
        | Some (_, loaded_cmt) ->
            if !log then print_s [%sexp `decl_from_def_uid (uid : Uast.Shape.Uid.t)];
            Shape.Uid.Tbl.find_opt loaded_cmt.infos.cmt_uid_to_decl uid)

  let sigitem_from_def_uid t (uid, impl_or_intf) =
    match decl_from_def_uid t (uid, impl_or_intf) with
    | Some item_decl -> Some (`Decl item_decl)
    | None ->
        Option.bind (comp_unit_of_uid uid) ~f:(fun comp_unit ->
            (* If there's no cmt, try the .cmi instead. This might help in the case of
                opam packages installed without .cmt. *)
            match loaded_cmi t ~comp_unit with
            | None -> None
            | Some (_, loaded_cmi) ->
                if !log then print_s [%sexp `decl_from_def_uid (uid : Uast.Shape.Uid.t)];
                Types.Uid.Tbl.find_opt (force loaded_cmi.defs) uid)
        |> Option.map ~f:(fun s -> `Sigitem s)

  type find_decl_result =
    (Shape.Uid.t * Typedtree.item_declaration option, string Lazy.t) Result.t

  let decl_from_reduce_for_uid_result t = function
    | Shape_reduce.Resolved uid | Resolved_alias (uid, _) ->
        Ok (uid, decl_from_def_uid t (uid, `impl))
    | res ->
        Error
          (lazy
            ("shape isn't resolved:"
            ^ "\n"
            ^ Sexp.to_string_hum [%sexp (res : Uast.Shape_reduce.result)]
            ^ "\n"
            ^ Format.asprintf "%a" Shape_reduce.print_result res))

  let decl_from_following_shape t (comp_unit, shape) =
    match loaded_cmt `impl t ~comp_unit with
    | None ->
        Error (lazy (Sexp.to_string_hum [%sexp "no comp unit", (comp_unit : string)]))
    | Some (_, loaded_cmt) ->
        let shape_res =
          let module M = (val loaded_cmt.m) in
          M.reduce_for_uid Env.empty shape
        in
        decl_from_reduce_for_uid_result t shape_res

  let shape_from_occurrence t (comp_unit, idloc) =
    match loaded_cmt `impl t ~comp_unit with
    | None -> None
    | Some (_, loaded_cmt) ->
        Map.find (force loaded_cmt.ident_occurrences) idloc
        |> Option.map ~f:(fun shape ->
               let shape =
                 lazy
                   (match shape with
                   | Resolved _ | Resolved_alias _ | Approximated _
                   | Internal_error_missing_uid ->
                       shape
                   | Unresolved shape ->
                       let module M = (val loaded_cmt.m) in
                       M.reduce_for_uid Env.empty shape)
               in
               decl_from_reduce_for_uid_result t (force shape))
end

module Type_index = struct
  type any_pattern = T : _ Typedtree.general_pattern -> any_pattern

  type t =
    { expr : (Typedtree.expression[@sexp.opaque]) list Hashtbl.M(Uast.Location).t
    ; pat : (any_pattern[@sexp.opaque]) list Hashtbl.M(Uast.Location).t
    ; constr :
        (Types.constructor_description[@sexp.opaque]) list Hashtbl.M(Uast.Location).t
    ; typ : (Typedtree.core_type[@sexp.opaque]) list Hashtbl.M(Uast.Location).t
    ; ce : (Typedtree.class_expr[@sexp.opaque]) list Hashtbl.M(Uast.Location).t
    ; cty : (Typedtree.class_type[@sexp.opaque]) list Hashtbl.M(Uast.Location).t
    }
  [@@deriving sexp_of]

  let create_from_cmt_infos (cmt_infos : Cmt_format.cmt_infos) (listing1 : Listing.one) =
    Load_path.init ~auto_include:Load_path.no_auto_include ~visible:[] ~hidden:[];
    List.iter listing1.cmt_load_paths ~f:(fun (lazy dir) -> Load_path.append_dir dir);
    match cmt_infos.cmt_annots with
    | Implementation structure ->
        let h_expr = Hashtbl.create (module Uast.Location.Ignoring_filename) in
        let h_pat = Hashtbl.create (module Uast.Location.Ignoring_filename) in
        let h_constr = Hashtbl.create (module Uast.Location.Ignoring_filename) in
        let h_typ = Hashtbl.create (module Uast.Location.Ignoring_filename) in
        let h_ce = Hashtbl.create (module Uast.Location.Ignoring_filename) in
        let h_cty = Hashtbl.create (module Uast.Location.Ignoring_filename) in
        let super = Tast_iterator.default_iterator in
        let self =
          { super with
            expr =
              (fun self expr ->
                super.expr self expr;
                Hashtbl.add_multi h_expr ~key:expr.exp_loc ~data:expr)
          ; pat =
              (fun (type k) self (pat : k Typedtree.general_pattern) ->
                (match pat.pat_desc with
                | Tpat_construct (id, constr_desc, _, _) ->
                    Hashtbl.add_multi h_constr ~key:id.loc ~data:constr_desc
                | _ -> ());
                super.pat self pat;
                Hashtbl.add_multi h_pat ~key:pat.pat_loc ~data:(T pat))
          ; typ =
              (fun self typ ->
                super.typ self typ;
                Hashtbl.add_multi h_typ ~key:typ.ctyp_loc ~data:typ)
          ; class_expr =
              (fun self ce ->
                super.class_expr self ce;
                Hashtbl.add_multi h_ce ~key:ce.cl_loc ~data:ce)
          ; class_type =
              (fun self ct ->
                super.class_type self ct;
                Hashtbl.add_multi h_cty ~key:ct.cltyp_loc ~data:ct)
          }
        in
        self.structure self structure;
        { expr = h_expr
        ; pat = h_pat
        ; constr = h_constr
        ; typ = h_typ
        ; ce = h_ce
        ; cty = h_cty
        }
    | Partial_implementation _ ->
        failwith "unexpected content of cmt (file doesn't fully type?)"
    | _ -> failwith "unexpected content of cmt"

  let create cmt_path listing1 = create_from_cmt_infos (read_cmt cmt_path) listing1
  let expr t loc = Hashtbl.find_multi t.expr loc
  let pat t loc = Hashtbl.find_multi t.pat loc
  let constr t loc = Hashtbl.find_multi t.constr loc
  let typ t loc = Hashtbl.find_multi t.typ loc
  let ce t loc = Hashtbl.find_multi t.ce loc
  let cty t loc = Hashtbl.find_multi t.cty loc
end
