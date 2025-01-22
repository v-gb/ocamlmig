open Base
open Stdio
open Common

type file_dep =
  | External of string
  | In_build_dir of string
  | In_source_tree of string
[@@deriving sexp]

type dep =
  | File of file_dep
  | Glob of Sexp.t
[@@deriving sexp]

type targets =
  { directories : string list
  ; files : string list
  }
[@@deriving sexp]

let __ t = t.directories

type rule_output =
  { deps : dep list
  ; targets : targets
  ; action : Sexp.t
  }
[@@deriving sexp] [@@sexp.allow_extra_fields]

let display (targets, rules) =
  let b = Buffer.create 10 in
  Printf.bprintf b "digraph G {\n";
  let rule_is_interesting rule =
    List.exists rule.deps ~f:(function
      | File (In_build_dir _) -> true
      | File (External _ | In_source_tree _) -> false
      | Glob _ -> true)
  in
  Hashtbl.iteri targets ~f:(fun ~key:target ~data:_ ->
      Printf.bprintf b "  %S [label=%S];\n" target
        (match String.chop_prefix target ~prefix:"_build/default/" with
        | Some s -> "^" ^ s
        | None -> target));
  Hashtbl.iteri rules ~f:(fun ~key:i ~data:rule ->
      if rule_is_interesting rule
      then
        Printf.bprintf b "  r%d [label=%S];\n" i
          (if true then "" else Sexp.to_string rule.action));
  Hashtbl.iteri rules ~f:(fun ~key:i ~data:rule ->
      if rule_is_interesting rule
      then (
        List.iter rule.targets.files ~f:(fun target ->
            Printf.bprintf b "  r%d -> %S;\n" i target);
        List.iter rule.deps ~f:(function
          | File (External _) -> ()
          | File (In_build_dir f | In_source_tree f) ->
              Printf.bprintf b "  %S -> r%d;\n" f i
          | Glob _ -> Printf.bprintf b "  \"glob\" -> r%d;\n" i)));
  Printf.bprintf b "}\n";
  Buffer.contents b

let build roots =
  let targets = Hashtbl.create (module String) in
  let rules = Hashtbl.create (module Int) in
  let to_visit = ref (Set.of_list (module String) roots) in
  while
    if Set.is_empty !to_visit
    then false
    else
      let text =
        run_process
          ([ "dune"; "describe"; "rules"; "--display"; "quiet"; "--" ]
          @ Set.to_list !to_visit)
      in
      to_visit := Set.empty (module String);
      let rule_outputs = Sexplib.Sexp.of_string_many_conv_exn text rule_output_of_sexp in
      List.iter rule_outputs ~f:(fun rule ->
          let i = Hashtbl.length rules in
          let inserted, not_inserted =
            List.partition_tf rule.targets.files ~f:(fun target ->
                (* If you ask for foo/a.ml, dune gives the rule for
                   _build/default/foo/a.ml, which results in redundant
                   rules. *)
                match Hashtbl.add targets ~key:target ~data:i with
                | `Ok -> true
                | `Duplicate -> false)
          in
          assert (List.is_empty inserted || List.is_empty not_inserted);
          if not (List.is_empty inserted) then Hashtbl.add_exn rules ~key:i ~data:rule;
          List.iter rule_outputs ~f:(fun rule ->
              List.iter rule.deps ~f:(function
                | File (External _ | In_source_tree _) -> ()
                | File (In_build_dir f) ->
                    if not (Hashtbl.mem targets f) then to_visit := Set.add !to_visit f
                | Glob _ -> ())));
      true
  do
    ()
  done;
  (targets, rules)

let dot roots = display (build roots)

let cmd =
  ( "dune-graph"
  , Command.basic ~summary:""
      [%map_open.Command
        let targets = anon (sequence ("TARGETS" %: Filename_unix.arg_type))
        and format =
          flag_optional_with_default_doc "format"
            (Arg_type.of_alist_exn [ ("dot", `Dot); ("svg", `Svg) ])
            [%sexp_of: [ `Dot | `Svg ]] ~default:`Svg ~doc:""
        and write = flag "-write" (optional Filename_unix.arg_type) ~doc:"" in
        fun () ->
          let print =
            match write with
            | None -> print_string
            | Some f -> Out_channel.write_all f ~data:__
          in
          let dot = dot targets in
          match format with
          | `Dot -> print dot
          | `Svg ->
              let svg =
                with_str_in_file dot (fun fname ->
                    run_process [ "dot"; "-T"; "svg"; no_dash (Abspath.to_string fname) ])
              in
              print svg] )
