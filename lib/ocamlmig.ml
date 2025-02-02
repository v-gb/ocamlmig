open Base
open Stdio
module Filename = Stdlib.Filename

let ( ^/ ) = Filename.concat

module Sys = Stdlib.Sys
module Printf = Stdlib.Printf
module Format = Stdlib.Format
open Common

module Vcs = struct
  type root =
    { system : [ `Git | `Hg | `JJ ]
    ; abs : string
    ; rel : string
    }

  let __ t = t.abs

  let rec root abs rel =
    match
      List.find_map
        ~f:(fun (f, v) -> if f () then Some v else None)
        [ ((fun () -> Sys.file_exists (abs ^/ ".git")), `Git)
        ; ((fun () -> Sys.file_exists (abs ^/ ".hg")), `Hg)
        ; ((fun () -> Sys.file_exists (abs ^/ ".jj")), `JJ)
        ]
    with
    | Some system -> Some { system; abs; rel }
    | None ->
        if Filename.dirname abs =: abs
        then None
        else
          root (Filename.dirname abs)
            (if rel =: "." then ".." else Filename.concat rel "..")

  let root () = root (Sys.getcwd ()) "."

  let root_exn () =
    match root () with
    | Some r -> r
    | None -> raise_s [%sexp "couldn't find a vcs repository (git/jj/hg)"]

  let files t =
    transient_line "loading files";
    (match t.system with
    | `JJ -> run_process [ "jj"; "file"; "list" ] |> String.split_lines
    | `Hg -> run_process [ "hg"; "files" ] |> String.split_lines
    | `Git -> run_process [ "git"; "ls-files"; "--"; t.rel ] |> String.split_lines)
    |> Cwdpath.create_list
end

let diff ?label1 ?label2 src1 src2 =
  let label1 =
    Option.value label1
      ~default:(match src1 with `Path p -> Cwdpath.to_string p | `Str _ -> "")
  in
  let label2 =
    Option.value label2
      ~default:(match src2 with `Path p -> Cwdpath.to_string p | `Str _ -> "")
  in
  let with_file src f =
    match src with
    | `Path p -> f p
    | `Str s -> with_str_in_file s (fun p -> f (Abspath.to_cwdpath p))
  in
  with_file src1 (fun path1 ->
      with_file src2 (fun path2 ->
          flush_transient ();
          let code =
            Sys.command
              (match Sys.getenv_opt "OCAMLMIG_DIFF" with
              | Some script ->
                  Printf.sprintf "LABEL1=%s; LABEL2=%s; %s %s %s" (Filename.quote label1)
                    (Filename.quote label2) script
                    (quote_no_dash (Cwdpath.to_string path1))
                    (quote_no_dash (Cwdpath.to_string path2))
              | None ->
                  Printf.sprintf
                    "if which patdiff >/dev/null; then patdiff -context 5 -alt-old %s \
                     -alt-new %s %s %s; else diff -u --label %s --label %s %s %s; fi"
                    (Filename.quote label1) (Filename.quote label2)
                    (quote_no_dash (Cwdpath.to_string path1))
                    (quote_no_dash (Cwdpath.to_string path2))
                    (Filename.quote label1) (Filename.quote label2)
                    (quote_no_dash (Cwdpath.to_string path1))
                    (quote_no_dash (Cwdpath.to_string path2)))
          in
          if code <> 0 && code <> 1 then failwith "diffing failed"))

let diff_or_write ~fmt_ocaml file_path ~write (file_contents, file_contents') =
  let file_contents =
    lazy
      (match fmt_ocaml with
      | Some (`Even_orig true) ->
          Dyn_ocamlformat.format ~path:file_path ~contents:file_contents
      | None | Some (`Even_orig false) -> file_contents)
  in
  let file_contents' =
    if Option.is_some fmt_ocaml
    then Dyn_ocamlformat.format ~path:file_path ~contents:file_contents'
    else file_contents'
  in
  if write
  then Out_channel.write_all (Cwdpath.to_string file_path) ~data:file_contents'
  else
    diff ~label1:(Cwdpath.to_string file_path) ~label2:(Cwdpath.to_string file_path)
      (`Str (force file_contents))
      (`Str file_contents')

let cwdpath_param = Command.Arg_type.map Filename_unix.arg_type ~f:Cwdpath.create

let source_param =
  [%map_open.Command
    let args = anon (sequence ("SOURCE_PATH" %: cwdpath_param))
    and more_args =
      flag "--"
        (map_flag escape ~f:(Option.map ~f:Cwdpath.create_list))
        ~doc:
          "SOURCE_PATH same as anonymous arguments. If no .ml files are specified, all \
           .ml files under version control are chosen."
    in
    match args @ Option.value more_args ~default:[] with
    | [] -> `All
    | _ :: _ as l -> `List l]

let file_paths_of_source = function
  | `List l -> l
  | `All ->
      Vcs.files (Vcs.root_exn ())
      |> List.filter ~f:(fun p -> String.is_suffix ~suffix:".ml" p.p)

let find_dune_root () =
  (* None of the dune commands are usable if you have a dune already running, so hand
     roll this. *)
  let dir = ref (Sys.getcwd ()) in
  while (not (Sys.file_exists (!dir ^/ "_build"))) && !dir <>: "/" do
    dir := Filename.dirname !dir
  done;
  if !dir =: "/" then Error "can't find dune" else Ok (Abspath.create_exn !dir)

let wrap str =
  let width = 80 in
  str
  |> String.split_lines
  |> List.map ~f:(function
       | "" -> ""
       | line ->
           let line_rest = String.lstrip line in
           let line_indent =
             String.prefix line (String.length line - String.length line_rest)
           in
           String.split line_rest ~on:' '
           |> (let size = ref 0 in
               List.group ~break:(fun elt _ ->
                   size := !size + 1 + String.length elt;
                   if !size > width
                   then (
                     size := 0;
                     true)
                   else false))
           |> List.map ~f:(fun l -> line_indent ^ String.concat ~sep:" " l)
           |> String.concat ~sep:"\n")
  |> String.concat_lines

let load_ocamlformat_conf ~dune_root ~for_file =
  let to_fpath str =
    match Ocamlformat_stdlib.Fpath.of_string str with
    | Ok v -> v
    | Error (`Msg s) -> failwith s
  in
  Bin_conf.build_config
    ~enable_outside_detected_project:true
      (* Not sure if this setting is necessary, but the intention is for people to be
         able to check out this tool even if ocamlformat isn't in use. *)
    ~root:(Some (to_fpath (Abspath.to_string dune_root)))
    ~file:(Cwdpath.to_string for_file) ~is_stdin:false
  |> Result.ok_or_failwith

let flag_optional_with_default_doc_custom name ~all:all_v ~to_string ~default ~doc =
  let open Command.Let_syntax.Let_syntax.Open_on_rhs in
  flag name
    (optional_with_default default
       (Arg_type.of_alist_exn ~list_values_in_help:false
          (List.map all_v ~f:(fun v -> (to_string v, v)))))
    ~doc:
      (Printf.sprintf "%s %s (default: %s)"
         (String.concat ~sep:"|" (List.map ~f:to_string all_v))
         doc (to_string default))

let ocamlformat_conf_param =
  [%map_open.Command
    let unformatted =
      flag_optional_with_default_doc_custom "unformatted" ~all:[ `Skip; `Fail; `Proceed ]
        ~to_string:(function `Skip -> "skip" | `Fail -> "fail" | `Proceed -> "proceed")
        ~default:`Fail ~doc:"what to do with .ml files not configured with ocamlformat"
    in
    fun ~dune_root file_path ->
      let fmconf = load_ocamlformat_conf ~dune_root ~for_file:file_path in
      let fmconf, fm_orig =
        if not fmconf.opr_opts.disable.v
        then (fmconf, false)
        else
          match unformatted with
          | `Proceed ->
              ( Ocamlformat_lib.Conf.Operational.update fmconf ~f:(fun opr_opts ->
                    { opr_opts with
                      disable = Ocamlformat_lib.Conf_t.Elt.make false `Default
                    })
              , true )
          | `Skip ->
              eprintf "warning: ocamlformat disabled for %s\n%!"
                (Cwdpath.to_string file_path);
              (fmconf, false)
          | `Fail -> failwith ("ocamlformat disabled for " ^ Cwdpath.to_string file_path)
      in
      if not fmconf.opr_opts.disable.v
      then
        (* We're using the same config as the user, but what we want is probably the
           config that causes the least amount of changes, because even though we use
           the same config settings as the user, we may not use the same version of
           ocamlformat, so the defaults can differ, so static ocamlformat can make a
           different choice from dynamic ocamlformat, and if the dynamic one preserves
           that choice, then we have randomly modified the source code (example:
           ocamlformat 0.27.0 wraps doc comments by default, where 0.26.2 doesn't. So
           if 0.27.0 is statically linked in and the user repo is formatted by 0.26.2,
           we ends up wrapping the user's doc comments). *)
        let try_update conf ~name ~value =
          match Ocamlformat_lib.Conf.update_value conf ~name ~value with
          | Error _ -> conf
          | Ok conf -> conf
        in
        Some
          ( fmconf
            |> try_update ~name:"wrap-comments" ~value:"false"
            |> try_update ~name:"wrap-docstrings" ~value:"false"
            |> try_update ~name:"parse-docstrings" ~value:"false"
          , `Even_orig fm_orig )
      else None]

let write_param =
  let open Command.Let_syntax.Let_syntax.Open_on_rhs in
  flag "-w" no_arg
    ~doc:
      "instead of showing the diff of the changes, writing the changes to the source \
       files"

let make_report_exn () =
  let got_error = ref false in
  ( got_error
  , fun e ->
      match Uast.Location.report_exception Format.err_formatter e with
      | () -> got_error := true
      | exception e -> (
          match Fmast.Location.report_exception Format.err_formatter e with
          | () -> got_error := true) )

let with_ocaml_exn f =
  let got_error, report_exn = make_report_exn () in
  (try f report_exn with e -> report_exn e);
  if !got_error then Stdlib.exit 1

let with_reported_ocaml_exn report_exn def f =
  (* this is to avoid stopping the processing on parse errors. In particular, the
     Gillian repo had toplevel file with #use directive, which can't be parsed as mere
     structures. *)
  try f ()
  with e ->
    report_exn e;
    def

let migrate =
  ( "migrate"
  , Command.basic ~summary:"Update code according to [@migrate] attributes"
      [%map_open.Command
        let source = source_param
        and write = write_param
        and get_ocamlformat_conf = ocamlformat_conf_param
        and side_migrations_libraries =
          flag "-side-migrations"
            ~doc:
              "library_name The name of a library in which ocamlmig will look for \
               attributes [let _ = source_function [@migrate ...]], e.g. \
               ocamlmig.stdlib_to_stdlib."
            (optional string)
        in
        fun () ->
          with_ocaml_exn (fun report_exn ->
              let source_paths = file_paths_of_source source in
              let dune_root = find_dune_root () |> Result.ok_or_failwith in
              transient_line "preparing artifacts";
              let listing = Build.Listing.create ~dune_root ~source_paths in
              let deps = Queue.create () in
              let artifacts_cache = Build.Artifacts.create_cache () in
              List.iter source_paths ~f:(fun source_path ->
                  transient_line
                    (Printf.sprintf "processing %s" (Cwdpath.to_string source_path));
                  match get_ocamlformat_conf ~dune_root source_path with
                  | None -> ()
                  | Some (fmconf, fm_orig) -> (
                      match Build.Listing.locate_cmt listing ~source_path with
                      | Error e ->
                          eprint_s
                            [%sexp
                              ("skipping " ^ Cwdpath.to_string source_path : string)
                            , (e : Sexp.t)]
                      | Ok (cmt_path, listing1) ->
                          let type_index = Build.Type_index.create cmt_path listing1 in
                          let cmt_infos = Build.read_cmt cmt_path in
                          let artifacts =
                            ( cmt_infos.cmt_modname
                            , Build.Artifacts.create ~cache:artifacts_cache listing )
                          in
                          let side_migrations_cmts =
                            Option.map side_migrations_libraries ~f:(fun library_name ->
                                match
                                  Build.Artifacts.locate_cmt_from_library_name
                                    (snd artifacts) ~dune_root ~library_name
                                with
                                | Some v -> v
                                | None ->
                                    raise_s
                                      [%sexp
                                        "unable to find cmt for"
                                      , ~~(library_name : string)])
                          in
                          with_reported_ocaml_exn report_exn None (fun () ->
                              Transform_migration.run ~fmconf ~artifacts ~source_path
                                ~side_migrations_cmts ~type_index
                                ~input_name_matching_compilation_command:
                                  (Build.input_name_matching_compilation_command cmt_infos))
                          |> Option.iter ~f:(fun (contents, { libraries }) ->
                                 Queue.enqueue deps (`Path source_path, libraries);
                                 diff_or_write ~fmt_ocaml:(Some fm_orig) source_path
                                   ~write contents)));
              List.iter
                (Dune_files.add_dependencies ~dune_root (Queue.to_list deps))
                ~f:(fun (`Path file_path, before, after) ->
                  diff_or_write ~fmt_ocaml:None file_path ~write (before, after)))] )

type transform_ctx =
  { source_path : Cwdpath.t
  ; cmt_path : Cwdpath.t
  ; cmt_infos : Cmt_format.cmt_infos Lazy.t
  ; listing : Build.Listing.t
  ; type_index : Build.Type_index.t Lazy.t
  ; ocamlformat_conf : (Ocamlformat_lib.Conf_t.t * [ `Even_orig of bool ]) option Lazy.t
  ; diff_or_write :
      fmt_ocaml:[ `Even_orig of bool ] option -> (string * string) option -> unit
  ; artifacts_cache : Build.Artifacts.cache
  }

let make_transform param =
  [%map_open.Command
    let source = source_param
    and write = write_param
    and get_ocamlformat_conf = ocamlformat_conf_param
    and f = param in
    fun () ->
      with_ocaml_exn (fun report_exn ->
          let source_paths = file_paths_of_source source in
          transient_line "preparing artifacts";
          let dune_root = find_dune_root () in
          let listing =
            match dune_root with
            | Ok dune_root -> Build.Listing.create ~dune_root ~source_paths
            | Error _ -> Build.Listing.create_without_dune (Abspath.cwd ())
          in
          let artifacts_cache = Build.Artifacts.create_cache () in
          List.iter source_paths ~f:(fun source_path ->
              transient_line
                (Printf.sprintf "processing %s" (Cwdpath.to_string source_path));
              let ocamlformat_conf =
                lazy
                  (get_ocamlformat_conf
                     ~dune_root:(Result.ok_or_failwith dune_root)
                     source_path)
              in
              let diff_or_write ~fmt_ocaml =
                Option.iter __ ~f:(diff_or_write ~fmt_ocaml source_path ~write)
              in
              match Build.Listing.locate_cmt listing ~source_path with
              | Error e ->
                  eprint_s
                    [%sexp
                      ("skipping " ^ Cwdpath.to_string source_path : string), (e : Sexp.t)]
              | Ok (cmt_path, dirs) ->
                  let cmt_infos = lazy (Build.read_cmt cmt_path) in
                  let type_index =
                    lazy (Build.Type_index.create_from_cmt_infos (force cmt_infos) dirs)
                  in
                  with_reported_ocaml_exn report_exn () (fun () ->
                      f
                        { source_path
                        ; cmt_path
                        ; cmt_infos
                        ; listing
                        ; type_index
                        ; ocamlformat_conf
                        ; diff_or_write
                        ; artifacts_cache
                        })))]

let transform =
  ( "transform"
  , Command.group ~summary:"Update code using one of several transformations"
      [ ( "strict-sequence"
        , Command.basic
            ~summary:"rewrite (a; b) into (ignore a; b) when a doesn't have type unit"
            ~readme:(fun () ->
              wrap
                "This is a possible way to automatically make code compatible with \
                 -strict-sequence.\n\n\
                 Limitations:\n\
                 - assumes \"ignore\" is not shadowed")
            (make_transform
               [%map_open.Command
                 let () = return () in
                 fun ctx ->
                   Transform_strict_sequence.run ~type_index:(force ctx.type_index)
                     ctx.source_path
                   |> ctx.diff_or_write ~fmt_ocaml:None]) )
      ; ( "rescope"
        , Command.basic ~summary:"transform scopes, e.g. by removing opens"
            ~readme:(fun () ->
              wrap
                "You have to pass either -unopen, -open or -unqualify to specify which \
                 transform you want.\n\n\
                 Limitations:\n\
                 - specified modules must be toplevel (i.e. correspond to files)\n\
                 - when identifiers are requalified, not all possible shadowings are \
                 taken into account, so complicated cases when end up with type errors\n\
                 - not all scopes are implemented. Identifiers to values are always \
                 implemented, and most others (identifiers of types, modules, module \
                 types, constructors, exception constructors) are not.\n")
            (make_transform
               [%map_open.Command
                 let transform =
                   let root_and_conservative str =
                     match String.split str ~on:' ' with
                     | [ str ] -> (str, false)
                     | [ str; "conservative" ] -> (str, true)
                     | _ ->
                         failwith
                           (Printf.sprintf "don't understand command line argument %s" str)
                   in
                   choose_one_non_optional ~if_nothing_chosen:Raise
                     [ flag "-unopen" (required string)
                         ~doc:
                           "Mod remove [open Mod] from the code, turning x into Mod.x \
                            where necessary to preserve behavior. Currently [Mod] must \
                            be a root module (i.e. a compilation unit)."
                       |> map ~f:(fun v ->
                              let name, conservative = root_and_conservative v in
                              Transform_scope.Unopen { name; conservative })
                     ; flag "-open" (required string)
                         ~doc:
                           "Mod|Mod! add either [open Mod] or [open! Mod] as the first \
                            line of the file, then modify the rest of the program to \
                            preserve behavior."
                       |> map ~f:(fun s ->
                              let s, conservative = root_and_conservative s in
                              let s, bang =
                                match String.chop_suffix s ~suffix:"!" with
                                | None -> (s, false)
                                | Some rest -> (rest, true)
                              in

                              Transform_scope.Open { name = s; bang; conservative })
                     ; flag "-unqualify"
                         (required (Arg_type.comma_separated string))
                         ~doc:
                           "Mod,... turn any Mod.x identifier in the source into x, so \
                            long as behavior is preserved."
                       |> map ~f:(fun v -> Transform_scope.Unqualify v)
                     ]
                 in
                 fun ctx ->
                   match force ctx.ocamlformat_conf with
                   | None -> ()
                   | Some (fmconf, fm_orig) ->
                       let artifacts =
                         Build.Artifacts.create ~cache:ctx.artifacts_cache ctx.listing
                       in
                       Transform_scope.run transform ~fmconf ~artifacts
                         ~type_index:(force ctx.type_index) ~source_path:ctx.source_path
                         ~input_name_matching_compilation_command:
                           (Build.input_name_matching_compilation_command
                              (force ctx.cmt_infos))
                         ~cmt_infos:(Build.read_cmt ctx.cmt_path)
                       |> ctx.diff_or_write ~fmt_ocaml:(Some fm_orig)]) )
      ; ( "migration-inverse"
        , Command.basic
            ~summary:
              "turn let _ = foo [@migrate ...] attributes into attributes that represent \
               the inverse migration."
            ~readme:(fun () ->
              wrap
                "For instance, given:\n\n\
                \  let _ = Foo.old [@@migrate { repl = (fun a b -> Foo.new_ ~a ~b) }]\n\n\
                 inverting the migration would result in\n\n\
                \  let _ = Foo.new_ [@@migrate { repl = (fun ~a ~b -> Foo.old a b) }]\n\n\
                 Limitations:\n\
                 - The current implementation only succeeds in very easy cases.\n")
            (make_transform
               [%map_open.Command
                 let () = return () in
                 fun ctx ->
                   match force ctx.ocamlformat_conf with
                   | None -> ()
                   | Some (fmconf, fm_orig) ->
                       Transform_migration_inverse.run ~fmconf
                         ~source_path:ctx.source_path
                         ~input_name_matching_compilation_command:
                           (Build.input_name_matching_compilation_command
                              (force ctx.cmt_infos))
                       |> ctx.diff_or_write ~fmt_ocaml:(Some fm_orig)]) )
      ; ( "migration-check"
        , Command.basic
            ~summary:
              "turn let _ = foo [@migrate ...] attributes into code that checks that the \
               replacement code types."
            ~readme:(fun () ->
              wrap
                "For instance, on:\n\n\
                \  let _ = List.map [@migrate { repl = fun f l -> ListLabel ~f l }]\n\n\
                 the checking code would be:\n\n\
                \  let _ = [ List.map; (fun f l -> ListLabel ~f l) ]\n\n")
            (make_transform
               [%map_open.Command
                 let () = return () in
                 fun ctx ->
                   match force ctx.ocamlformat_conf with
                   | None -> ()
                   | Some (fmconf, fm_orig) ->
                       Transform_migration_check.run ~fmconf ~source_path:ctx.source_path
                         ~input_name_matching_compilation_command:
                           (Build.input_name_matching_compilation_command
                              (force ctx.cmt_infos))
                       |> ctx.diff_or_write ~fmt_ocaml:(Some fm_orig)]) )
      ; ( "migration-to-typed"
        , Command.basic
            ~summary:
              "update let _ = foo [@migrate ...] attributes to use the let _ = [ foo; \
               repl ] [@migrate .. ] syntax."
            ~readme:(fun () ->
              wrap
                "This can be useful because the resulting syntax allows the typer to \
                 detect many errors at the definition of the [@migrate] attribute, \
                 (instead of when the rule fires, a source file is updated and now the \
                 resulting file is compiled).")
            (make_transform
               [%map_open.Command
                 let () = return () in
                 fun ctx ->
                   match force ctx.ocamlformat_conf with
                   | None -> ()
                   | Some (fmconf, fm_orig) ->
                       Transform_migration_to_typed.run ~fmconf
                         ~source_path:ctx.source_path
                         ~input_name_matching_compilation_command:
                           (Build.input_name_matching_compilation_command
                              (force ctx.cmt_infos))
                       |> ctx.diff_or_write ~fmt_ocaml:(Some fm_orig)]) )
      ] )

let substr_split t ~on:substr =
  let substr = String.Search_pattern.create substr in
  String.Search_pattern.split_on substr t

let replace =
  ( "replace"
  , Command.basic ~summary:"Update code in a sed-like way, but structured"
      ~readme:(fun () ->
        wrap
          "WARNING: this is currently very incomplete.\n\n\
           Example, to reorder the arguments of List.map:\n\
           ocamlmig replace -e 'List.map ~f:__f __l /// List.map __l ~f:__f'\n\n\
           PATTERN is an expression to be matched, where variables starting with __ are \
           placeholders.\n\
           REPL is the code to pattern a match with, with the placeholders replaced by \
           their matches in the pattern.")
      [%map_open.Command
        let patterns_and_repls =
          (* What we really want is -e PATTERN REPL, but command doesn't support that,
             and I don't think cmdliner does either. *)
          flag "-e" ~doc:"PATTERN /// REPL"
            (one_or_more_as_list
               (Arg_type.create (fun str ->
                    match substr_split str ~on:"///" with
                    | [ s1; s2 ] -> (s1, s2)
                    | _ ->
                        raise_s
                          [%sexp
                            "unexpected exactly one \"///\" in argument"
                          , (str : string)
                          , "to separate PATTERN and REPL"])))
        and source = source_param
        and write = write_param
        and get_ocamlformat_conf = ocamlformat_conf_param in
        fun () ->
          with_ocaml_exn (fun report_exn ->
              let source_paths = file_paths_of_source source in
              transient_line "preparing artifacts";
              let dune_root = find_dune_root () |> Result.ok_or_failwith in
              let listing = Build.Listing.create ~dune_root ~source_paths in
              let _artifacts_cache = Build.Artifacts.create_cache () in
              let transform_replace = Transform_replace.run patterns_and_repls () in
              List.iter source_paths ~f:(fun source_path ->
                  transient_line
                    (Printf.sprintf "processing %s" (Cwdpath.to_string source_path));
                  let located_cmt =
                    lazy (Build.Listing.locate_cmt listing ~source_path)
                  in
                  let cmt_infos =
                    Lazy.map located_cmt ~f:(function
                      | Error _ -> None
                      | Ok (cmt_path, _) -> Some (Build.read_cmt cmt_path))
                  in
                  let type_index =
                    lazy
                      (match (force located_cmt, force cmt_infos) with
                      | Error e, None ->
                          eprint_s
                            [%sexp
                              ("skipping over " ^ Cwdpath.to_string source_path : string)
                            , (e : Sexp.t)];
                          None
                      | Error _, Some _ | Ok _, None -> assert false
                      | Ok (_cmt_path, dirs), Some cmt_infos ->
                          Some (Build.Type_index.create_from_cmt_infos cmt_infos dirs))
                  in
                  match get_ocamlformat_conf ~dune_root source_path with
                  | None -> ()
                  | Some (fmconf, fm_orig) ->
                      with_reported_ocaml_exn report_exn None (fun () ->
                          transform_replace ~fmconf ~type_index ~source_path
                            ~input_name_matching_compilation_command:
                              (lazy
                                (Option.bind (force cmt_infos)
                                   ~f:Build.input_name_matching_compilation_command)))
                      |> Option.iter
                           ~f:(diff_or_write ~fmt_ocaml:(Some fm_orig) source_path ~write)))]
  )

let internal_dune_config =
  ( "dune-config"
  , Command.basic ~summary:""
      [%map_open.Command
        let source = source_param in
        fun () ->
          let source_paths = file_paths_of_source source in
          let dune_root = find_dune_root () |> Result.ok_or_failwith in
          print_s
            [%sexp (Build.Listing.create ~dune_root ~source_paths : Build.Listing.t)]] )

let internal_ocamlformat =
  ( "ocamlformat"
  , Command.basic ~summary:""
      [%map_open.Command
        let file_paths = anon (sequence ("SRC" %: cwdpath_param)) in
        fun () ->
          List.iter file_paths ~f:(fun file_path ->
              let str = In_channel.read_all (Cwdpath.to_string file_path) in
              let str' = Dyn_ocamlformat.format ~path:file_path ~contents:str in
              diff ~label1:(Cwdpath.to_string file_path)
                ~label2:(Cwdpath.to_string file_path) (`Str str) (`Str str'))] )

let internal_dune_files =
  ( "dune-files"
  , Command.basic ~summary:""
      [%map_open.Command
        let src, lib = anon (t2 ("SRC" %: cwdpath_param) ("LIB" %: string)) in
        fun () ->
          let dune_root = find_dune_root () |> Result.ok_or_failwith in
          List.iter
            (Dune_files.add_dependencies ~dune_root [ (`Path src, [ lib ]) ])
            ~f:(fun (`Path path, before, after) ->
              diff ~label1:(Cwdpath.to_string path) ~label2:(Cwdpath.to_string path)
                (`Str before) (`Str after))] )

let internal_parsetree =
  ( "parsetree"
  , Command.basic ~summary:""
      [%map_open.Command
        let code = anon ("CODE" %: string)
        and loc = flag "-loc" no_arg ~doc:""
        and upstream = flag "-u" no_arg ~doc:"" in
        fun () ->
          with_ocaml_exn (fun _ ->
              Ocamlformat_parser_shims.Clflags.locations := loc;
              Clflags.locations := loc;
              let dune_root = find_dune_root () |> Result.ok_or_failwith in
              if upstream
              then
                let ast = Uast.Parse.implementation (Lexing.from_string code) in
                Format.printf "%a@." Printast.implementation ast
              else
                let ast_with_comments =
                  Fmast.parse_with_ocamlformat Structure
                    ~conf:
                      (load_ocamlformat_conf ~dune_root ~for_file:(Cwdpath.create "z.ml"))
                    ~input_name:"z.ml" code
                in
                print_string (Fmast.debug_print ~raw:true Structure ast_with_comments.ast))]
  )

let internal_cmt =
  ( "cmt"
  , Command.basic ~summary:""
      [%map_open.Command
        let cmt_paths = anon (sequence ("CMT" %: cwdpath_param))
        and loc = flag "-loc" no_arg ~doc:"" in
        fun () ->
          Clflags.locations := loc;
          List.iter cmt_paths ~f:(fun cmt_path ->
              let cmt_infos = Build.read_cmt cmt_path in
              print_s
                [%sexp
                  `impl (cmt_infos.cmt_impl_shape : Uast.Shape.t option)
                , `occurrences
                    (cmt_infos.cmt_ident_occurrences
                      : (Uast.Longident.t Uast.Location.loc * Uast.Shape_reduce.result)
                        list)];
              Format.printf "%a@."
                (fun ppf tbl ->
                  Uast.Shape.Uid.Tbl.iter
                    (fun uid -> function
                      | Typedtree.Value value_description ->
                          Format.fprintf ppf "%a:@ %a" Shape.Uid.print uid
                            Uast.typed_print_value_description value_description
                      | _ -> ())
                    tbl)
                cmt_infos.cmt_uid_to_decl)] )

let internal_cmi =
  ( "cmi"
  , Command.basic ~summary:""
      [%map_open.Command
        let cmi_paths = anon (sequence ("CMI" %: cwdpath_param)) in
        fun () ->
          List.iter cmi_paths ~f:(fun cmi_path ->
              (* As it turns out, there is no function to print Types.signature, without
                 dropping attributes at least. *)
              let tree_of_value_description id (decl : Types.value_description) =
                let open Printtyp in
                let open Outcometree in
                let id = Ident.name id in
                let ty = tree_of_type_scheme decl.val_type in
                let vd =
                  { oval_name = id
                  ; oval_type = ty
                  ; oval_prims = []
                  ; oval_attributes =
                      (let uid_string =
                         Format.asprintf "%a" Shape.Uid.print decl.val_uid
                       in
                       { oattr_name = uid_string }
                       :: List.map decl.val_attributes ~f:(fun attr ->
                              { oattr_name = attr.attr_name.txt }))
                  }
                in
                let vd =
                  match decl.val_kind with Val_prim p -> Primitive.print p vd | _ -> vd
                in
                Osig_value vd
              in
              let cmi_infos =
                try Cmi_format.read_cmi (Cwdpath.to_string cmi_path)
                with Cmi_format.Error e ->
                  failwith (Format.asprintf "%a" Cmi_format.report_error e)
              in
              Format.printf "@[<v>%a@]@." !Oprint.out_signature
                (List.concat_map cmi_infos.cmi_sign ~f:(fun si ->
                     match si with
                     | Sig_value (id, decl, _) -> [ tree_of_value_description id decl ]
                     | _ -> Printtyp.tree_of_signature [ si ])))] )

let internal_untype =
  ( "untype"
  , Command.basic ~summary:""
      [%map_open.Command
        let code = anon ("CODE" %: string) in
        fun () ->
          with_ocaml_exn (fun _ ->
              Load_path.init ~auto_include:Load_path.no_auto_include
                ~visible:[ Config.standard_library ] ~hidden:[];
              let lexbuf = Lexing.from_string code in
              Lexing.set_filename lexbuf "command line";
              let structure, _, _, _, _ =
                Typemod.type_structure (Compmisc.initial_env ())
                  (Parse.implementation lexbuf)
              in
              Format.printf "%a@." Pprintast.structure
                (Untypeast.untype_structure structure))] )

let main () =
  let hidden ((name, _) as cmd) =
    if Array.length Sys.argv >= 2 && Sys.argv.(1) =: name then Some cmd else None
  in
  Command_unix.run
    (Command.group ~summary:"A tool for rewriting ocaml code"
       (List.concat
          [ [ migrate; transform ]
          ; Option.to_list (hidden replace)
          ; Option.to_list
              (hidden
                 ( "internal"
                 , Command.group
                     ~summary:"functionality not for users, mostly for debugging"
                     [ Dune_graph.cmd
                     ; internal_cmi
                     ; internal_cmt
                     ; internal_dune_config
                     ; internal_dune_files
                     ; internal_ocamlformat
                     ; internal_parsetree
                     ; internal_untype
                     ] ))
          ]))
